use crate::CONTEXT;
use anyhow::Context;
use emqjs_data_structures::{ImportRequests, ValueKind};
use rkyv::Archive;
use rquickjs::FromJs;
use rquickjs::{IntoJs, Rest};
use std::sync::Mutex;

/// encoded Vec<ImportRequest>
#[no_mangle]
static EMQJS_ENCODED_IMPORT_REQUESTS: [u8; 10240] = [0; 10240];

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

struct ImportsCtx {
    import_requests: &'static <ImportRequests as Archive>::Archived,
    funcs: Box<[rquickjs::Persistent<rquickjs::Function<'static>>]>,
}

impl ImportsCtx {
    fn new<'js>(ctx: rquickjs::Ctx<'js>, imports: rquickjs::Object<'js>) -> rquickjs::Result<Self> {
        let import_requests = unsafe {
            rkyv::archived_root::<ImportRequests>(
                // careful: single `&` would read the entire array into stack
                std::ptr::read_volatile(&&EMQJS_ENCODED_IMPORT_REQUESTS).as_slice(),
            )
        };
        let funcs = import_requests
            .iter()
            .map(|i| {
                let func: rquickjs::Function = imports.get(i.name.as_str())?;
                Ok(rquickjs::Persistent::save(ctx, func))
            })
            .collect::<rquickjs::Result<_>>()?;

        Ok(ImportsCtx {
            import_requests,
            funcs,
        })
    }
}

// ideally this would be OnceCell but ImportsCtx is !Send, so we need a full Mutex
static IMPORTS_CTX: Mutex<Option<ImportsCtx>> = Mutex::new(None);

pub fn provide_imports<'js>(
    ctx: rquickjs::Ctx<'js>,
    imports: rquickjs::Object<'js>,
) -> rquickjs::Result<()> {
    *IMPORTS_CTX.lock().unwrap() = Some(ImportsCtx::new(ctx, imports)?);
    Ok(())
}

#[no_mangle]
extern "C" fn emqjs_invoke(index: usize) {
    let imports_ctx_lock = IMPORTS_CTX.lock().unwrap();
    let imports_ctx = imports_ctx_lock
        .as_ref()
        .expect("imports weren't provided yet");
    let req = &imports_ctx.import_requests[index];
    let func = &imports_ctx.funcs[index];
    CONTEXT
        .get()
        .expect("Context wasn't initialized yet")
        .with(|ctx| -> rquickjs::Result<()> {
            // todo: try to avoid this allocation
            let args = req
                .params
                .iter()
                .zip(unsafe { EMQJS_VALUE_SPACE.iter() })
                .map(|(ty, value)| match ty {
                    ValueKind::I32 => unsafe { value.i32 }.into_js(ctx),
                    ValueKind::I64 => unsafe { value.i64 }.into_js(ctx),
                    ValueKind::F32 => unsafe { value.f32 }.into_js(ctx),
                    ValueKind::F64 => unsafe { value.f64 }.into_js(ctx),
                })
                .collect::<rquickjs::Result<Vec<_>>>()?;
            let result = func.clone().restore(ctx)?.call((Rest(args),))?;
            if let Some(&result_type) = req.result.as_ref() {
                let result_dst = unsafe { EMQJS_VALUE_SPACE.get_unchecked_mut(0) };
                match result_type {
                    ValueKind::I32 => result_dst.i32 = i32::from_js(ctx, result)?,
                    ValueKind::I64 => result_dst.i64 = i64::from_js(ctx, result)?,
                    ValueKind::F32 => result_dst.f32 = f32::from_js(ctx, result)?,
                    ValueKind::F64 => result_dst.f64 = f64::from_js(ctx, result)?,
                }
            }
            Ok(())
        })
        .unwrap()
}
