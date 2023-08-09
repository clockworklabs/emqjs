use crate::data_structures::{
    ArchivedExport, FuncType, Module, ValueKind, EMQJS_ALT_STACK_LEN, EMQJS_VALUE_SPACE_LEN,
};
use crate::web_assembly::{Memory, Table};
use crate::{externs, with_active_ctx, Volatile};
use once_cell::sync::Lazy;
use once_cell::unsync::OnceCell;
use rkyv::Archive;
use rquickjs::function::Rest;
use rquickjs::{Ctx, FromJs, IntoJs};
use std::sync::atomic::AtomicBool;

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

#[no_mangle]
static mut EMQJS_ALT_STACK: [u8; EMQJS_ALT_STACK_LEN] = [0; EMQJS_ALT_STACK_LEN];

struct StackSwapToken;

impl StackSwapToken {
    fn new() -> Self {
        unsafe {
            externs::swap_stack();
        }
        Self
    }
}

impl Drop for StackSwapToken {
    fn drop(&mut self) {
        unsafe {
            externs::swap_stack();
        }
    }
}

#[no_mangle]
static EMQJS_HAD_EXCEPTION: AtomicBool = AtomicBool::new(false);

struct WasmCtx {
    module: &'static <Module as Archive>::Archived,
    imports: Box<[rquickjs::Persistent<rquickjs::Function<'static>>]>,
}

impl WasmCtx {
    fn new<'js>(
        ctx: rquickjs::Ctx<'js>,
        imports: rquickjs::Object<'js>,
    ) -> rquickjs::Result<(Self, rquickjs::Object<'js>)> {
        static MODULE_BYTES: Lazy<Vec<u8>> = Lazy::new(|| unsafe {
            let mut bytes = Vec::with_capacity(externs::encoded_module_len());
            externs::encoded_module(bytes.as_mut_ptr());
            bytes.set_len(externs::encoded_module_len());
            bytes
        });

        let module = unsafe { rkyv::archived_root::<Module>(&MODULE_BYTES) };

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
                    let func = wrap_export(ctx, &e.ty, move || {
                        // println!("Invoking export {i} (original name {name})", name = e.name);
                        unsafe { externs::invoke_export(i) }
                    })?;
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
                            wrap_export(ctx, ty, move || {
                                // println!("Invoking table {i}");
                                unsafe { externs::invoke_table(i) }
                            })
                            .map(Some)
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
            // println!("Passing args {:?}", &params.0);

            ty.params
                .iter()
                .zip(params.into_inner())
                .map(|(kind, value)| from_js(ctx, *kind, value))
                .zip(unsafe { EMQJS_VALUE_SPACE.iter_mut() })
                .try_for_each(|(value, slot)| value.map(|value| *slot = value))?;

            {
                // Swap stack to Wasm one for the duration of the call.
                let _token = StackSwapToken::new();
                invoke_callback();
            }

            if EMQJS_HAD_EXCEPTION.swap(false, std::sync::atomic::Ordering::SeqCst) {
                // rethrow exception;
                // actual exception is still stored in the context so this is equivalent to `Err(ctx.throw(ctx.catch()))`
                return Err(rquickjs::Error::Exception);
            }

            let result = ty
                .result
                .as_ref()
                .map(|kind| into_js(ctx, *kind, unsafe { EMQJS_VALUE_SPACE[0] }))
                .transpose();

            // println!("Got result {result:?}");

            result
        },
    )
}

thread_local! {
    static IMPORTS_CTX: OnceCell<WasmCtx> = OnceCell::new();
}

pub fn provide_imports<'js>(
    ctx: rquickjs::Ctx<'js>,
    imports: rquickjs::Object<'js>,
) -> rquickjs::Result<rquickjs::Object<'js>> {
    let (wasm_ctx, exports) = WasmCtx::new(ctx, imports)?;
    IMPORTS_CTX.with(|imports_ctx| {
        imports_ctx
            .set(wasm_ctx)
            .unwrap_or_else(|_| panic!("provide_imports was already called"))
    });
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
pub extern "C" fn emqjs_invoke_import(index: usize) -> bool {
    // Swap stack to the JS one for the duration of the call.
    let _token = StackSwapToken::new();
    let (ty, func) = IMPORTS_CTX.with(|imports_ctx| {
        let imports_ctx = imports_ctx.get().expect("imports weren't provided yet");
        let ty = &imports_ctx.module.imports[index].ty;
        let func = imports_ctx.imports[index].clone();
        (ty, func)
    });
    with_active_ctx(|ctx| -> rquickjs::Result<_> {
        // todo: try to avoid this allocation
        let args = ty
            .params
            .iter()
            .zip(unsafe { EMQJS_VALUE_SPACE.iter() })
            .map(|(ty, value)| into_js(ctx, *ty, *value))
            .collect::<rquickjs::Result<Vec<_>>>()?;
        // println!(
        //     "Invoking import {index} -> {func} with signature {sig:?} and args {args:?}",
        //     func = func
        //         .clone()
        //         .restore(ctx)?
        //         .as_object()
        //         .get::<_, String>("name")?,
        //     sig = ty,
        // );
        match func.clone().restore(ctx)?.call((Rest(args),)) {
            Ok(value) => {
                if let Some(&result_type) = ty.result.as_ref() {
                    let value = from_js(ctx, result_type, value)?;
                    unsafe {
                        *EMQJS_VALUE_SPACE.get_unchecked_mut(0) = value;
                    }
                }
                Ok(true)
            }
            Err(rquickjs::Error::Exception) => {
                // actual exception is stored in the context;
                // we only need to store our own flag because `ctx.catch()` returns JS `null` value
                // both in case of `throw null` and in case of no exception
                EMQJS_HAD_EXCEPTION.store(true, std::sync::atomic::Ordering::SeqCst);
                Ok(false)
            }
            Err(e) => Err(e),
        }
    })
    .unwrap()
}
