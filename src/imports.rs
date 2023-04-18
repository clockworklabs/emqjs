use crate::CONTEXT;
use anyhow::Context;
use emqjs_data_structures::{Module, ValueKind};
use rkyv::Archive;
use rquickjs::{Ctx, FromJs};
use rquickjs::{IntoJs, Rest};
use std::sync::Mutex;

/// encoded Vec<ImportRequest>
#[no_mangle]
static EMQJS_ENCODED_MODULE: [u8; 10240] = [0; 10240];

#[repr(C)]
#[derive(Clone, Copy)]
union Value {
    i32: i32,
    i64: i64,
    f32: f32,
    f64: f64,
}

#[no_mangle]
static mut EMQJS_VALUE_SPACE: [Value; 1024] = [Value { i64: 0 }; 1024];

struct WasmCtx {
    module: &'static <Module as Archive>::Archived,
    imports: Box<[rquickjs::Persistent<rquickjs::Function<'static>>]>,
    exports: Box<[rquickjs::Persistent<rquickjs::Function<'static>>]>,
}

impl WasmCtx {
    fn new<'js>(ctx: rquickjs::Ctx<'js>, imports: rquickjs::Object<'js>) -> rquickjs::Result<Self> {
        let module = unsafe {
            rkyv::archived_root::<Module>(
                // careful: single `&` would read the entire array into stack
                std::ptr::read_volatile(&&EMQJS_ENCODED_MODULE).as_slice(),
            )
        };

        let imports = module
            .imports
            .iter()
            .map(|i| {
                let func: rquickjs::Function = imports.get(i.name.as_str())?;
                Ok(rquickjs::Persistent::save(ctx, func))
            })
            .collect::<rquickjs::Result<_>>()?;

        let exports = module
            .exports
            .iter()
            .enumerate()
            .map(|(i, e)| {
                let func = rquickjs::Function::new(
                    ctx,
                    move |ctx: Ctx<'js>, params: Rest<rquickjs::Value<'js>>| {
                        e.ty.params
                            .iter()
                            .zip(params.into_inner())
                            .map(|(kind, value)| from_js(ctx, *kind, value))
                            .zip(unsafe { EMQJS_VALUE_SPACE.iter_mut() })
                            .try_for_each(|(value, slot)| value.map(|value| *slot = value))?;

                        unsafe { emqjs_invoke_export(i) };

                        e.ty.result
                            .as_ref()
                            .map(|kind| into_js(ctx, *kind, unsafe { EMQJS_VALUE_SPACE[0] }))
                            .transpose()
                    },
                )?;

                Ok(rquickjs::Persistent::save(ctx, func))
            })
            .collect::<rquickjs::Result<_>>()?;

        Ok(WasmCtx {
            module,
            imports,
            exports,
        })
    }
}

// ideally this would be OnceCell but ImportsCtx is !Send, so we need a full Mutex
static IMPORTS_CTX: Mutex<Option<WasmCtx>> = Mutex::new(None);

pub fn provide_imports<'js>(
    ctx: rquickjs::Ctx<'js>,
    imports: rquickjs::Object<'js>,
) -> rquickjs::Result<()> {
    *IMPORTS_CTX.lock().unwrap() = Some(WasmCtx::new(ctx, imports)?);
    Ok(())
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
extern "C" fn emqjs_invoke_import(index: usize) {
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
}
