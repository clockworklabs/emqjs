use crate::data_structures::{
    ArchivedExport, FuncType, Module, ValueKind, EMQJS_ENCODED_MODULE_LEN, EMQJS_VALUE_SPACE_LEN,
};
use crate::web_assembly::{Memory, Table};
use crate::{Volatile, CONTEXT};
use rkyv::Archive;
use rquickjs::{Ctx, FromJs};
use rquickjs::{IntoJs, Rest};
use std::sync::Mutex;

/// encoded Vec<ImportRequest>
#[no_mangle]
static EMQJS_ENCODED_MODULE: Volatile<[u8; EMQJS_ENCODED_MODULE_LEN]> =
    Volatile::new([0; EMQJS_ENCODED_MODULE_LEN]);

#[repr(C)]
#[derive(Clone, Copy)]
union Value {
    i32: i32,
    i64: i64,
    f32: f32,
    f64: f64,
}

#[no_mangle]
static mut EMQJS_VALUE_SPACE: Volatile<[Value; EMQJS_VALUE_SPACE_LEN]> =
    Volatile::new([Value { i64: 0 }; EMQJS_VALUE_SPACE_LEN]);

struct WasmCtx {
    module: &'static <Module as Archive>::Archived,
    imports: Box<[rquickjs::Persistent<rquickjs::Function<'static>>]>,
}

impl WasmCtx {
    fn new<'js>(
        ctx: rquickjs::Ctx<'js>,
        imports: rquickjs::Object<'js>,
    ) -> rquickjs::Result<(Self, rquickjs::Object<'js>)> {
        let module = unsafe { rkyv::archived_root::<Module>(&EMQJS_ENCODED_MODULE[..]) };

        let imports = module
            .imports
            .iter()
            .map(|i| {
                let func: rquickjs::Function = imports.get(i.name.as_str())?;
                Ok(rquickjs::Persistent::save(ctx, func))
            })
            .collect::<rquickjs::Result<_>>()?;

        let exports = rquickjs::Object::new(ctx)?;

        module
            .exports
            .iter()
            .enumerate()
            .try_for_each(|(i, e)| match e {
                ArchivedExport::Func(e) => {
                    let func = wrap_export(ctx, &e.ty, move || unsafe { emqjs_invoke_export(i) })?;
                    exports.set(e.name.as_str(), func)
                }
                ArchivedExport::Table { name, types } => {
                    let table = types
                        .iter()
                        .enumerate()
                        .map(|(i, ty)| {
                            let ty = match ty.as_ref() {
                                Some(ty) => ty,
                                None => return Ok(None),
                            };
                            wrap_export(ctx, ty, move || unsafe { emqjs_invoke_table(i) }).map(Some)
                        })
                        .collect::<rquickjs::Result<_>>()?;

                    exports.set(name.as_str(), Table::new(ctx, table))
                }
                ArchivedExport::Memory { name } => exports.set(name.as_str(), Memory),
            })?;

        Ok((WasmCtx { module, imports }, exports))
    }
}

fn wrap_export<'js>(
    ctx: Ctx<'js>,
    ty: &'static <FuncType as Archive>::Archived,
    invoke_callback: impl 'static + Fn() + Send,
) -> Result<rquickjs::Function<'js>, rquickjs::Error> {
    rquickjs::Function::new(
        ctx,
        move |ctx: Ctx<'js>, params: Rest<rquickjs::Value<'js>>| {
            ty.params
                .iter()
                .zip(params.into_inner())
                .map(|(kind, value)| from_js(ctx, *kind, value))
                .zip(unsafe { EMQJS_VALUE_SPACE.iter_mut() })
                .try_for_each(|(value, slot)| value.map(|value| *slot = value))?;

            invoke_callback();

            ty.result
                .as_ref()
                .map(|kind| into_js(ctx, *kind, unsafe { EMQJS_VALUE_SPACE[0] }))
                .transpose()
        },
    )
}

// ideally this would be OnceCell but ImportsCtx is !Send, so we need a full Mutex
static IMPORTS_CTX: Mutex<Option<WasmCtx>> = Mutex::new(None);

pub fn provide_imports<'js>(
    ctx: rquickjs::Ctx<'js>,
    imports: rquickjs::Object<'js>,
) -> rquickjs::Result<rquickjs::Object<'js>> {
    let (wasm_ctx, exports) = WasmCtx::new(ctx, imports)?;
    *IMPORTS_CTX.lock().unwrap() = Some(wasm_ctx);
    Ok(exports)
}

fn into_js(ctx: Ctx, kind: ValueKind, value: Value) -> rquickjs::Result<rquickjs::Value<'_>> {
    unsafe {
        match kind {
            ValueKind::I32 => value.i32.into_js(ctx),
            ValueKind::I64 => value.i64.into_js(ctx),
            ValueKind::F32 => value.f32.into_js(ctx),
            ValueKind::F64 => value.f64.into_js(ctx),
        }
    }
}

fn from_js<'js>(
    ctx: Ctx<'js>,
    kind: ValueKind,
    value: rquickjs::Value<'js>,
) -> rquickjs::Result<Value> {
    Ok(match kind {
        ValueKind::I32 => Value {
            i32: i32::from_js(ctx, value)?,
        },
        ValueKind::I64 => Value {
            i64: i64::from_js(ctx, value)?,
        },
        ValueKind::F32 => Value {
            f32: f32::from_js(ctx, value)?,
        },
        ValueKind::F64 => Value {
            f64: f64::from_js(ctx, value)?,
        },
    })
}

#[no_mangle]
pub extern "C" fn emqjs_invoke_import(index: usize) {
    let imports_ctx_lock = IMPORTS_CTX.lock().unwrap();
    let imports_ctx = imports_ctx_lock
        .as_ref()
        .expect("imports weren't provided yet");
    let ty = &imports_ctx.module.imports[index].ty;
    let func = &imports_ctx.imports[index];
    CONTEXT
        .get()
        .expect("Context wasn't initialized yet")
        .with(|ctx| -> rquickjs::Result<()> {
            // todo: try to avoid this allocation
            let args = ty
                .params
                .iter()
                .zip(unsafe { EMQJS_VALUE_SPACE.iter() })
                .map(|(ty, value)| into_js(ctx, *ty, *value))
                .collect::<rquickjs::Result<Vec<_>>>()?;
            let result = func.clone().restore(ctx)?.call((Rest(args),))?;
            if let Some(&result_type) = ty.result.as_ref() {
                let value = from_js(ctx, result_type, result)?;
                unsafe {
                    *EMQJS_VALUE_SPACE.get_unchecked_mut(0) = value;
                }
            }
            Ok(())
        })
        .unwrap()
}

extern "C" {
    fn emqjs_invoke_export(index: usize);
    fn emqjs_invoke_table(index: usize);
}
