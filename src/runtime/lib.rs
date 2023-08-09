#[path = "../data_structures.rs"]
mod data_structures;

mod externs;
mod imports;

use rquickjs::function::Rest;
use rquickjs::{bind, CatchResultExt, Context, Ctx, Function, HasRefs, Object, Runtime, Value};
use std::cell::Cell;

// Workaround for https://github.com/emscripten-core/emscripten/issues/19236.
// Won't properly work but
#[no_mangle]
static mut errno: i32 = 0;

struct Volatile<T> {
    data: T,
}

impl<T> Volatile<T> {
    pub const fn new(data: T) -> Self {
        Self { data }
    }
}

impl<T> std::ops::Deref for Volatile<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { std::ptr::read_volatile(&&self.data) }
    }
}

impl<T> std::ops::DerefMut for Volatile<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { std::ptr::read_volatile(&&mut self.data) }
    }
}

#[bind(object)]
mod console {
    use super::*;

    #[quickjs(skip)]
    fn output(mut dest: impl std::io::Write, args: Rest<Value>) -> rquickjs::Result<()> {
        let mut first = true;
        for arg in args.into_inner() {
            if first {
                first = false;
            } else {
                write!(dest, " ")?;
            }
            write!(dest, "{arg:?}")?;
        }
        writeln!(dest)?;
        Ok(())
    }

    pub fn log(args: Rest<Value>) -> rquickjs::Result<()> {
        output(std::io::stdout().lock(), args)
    }

    pub fn error(args: Rest<Value>) -> rquickjs::Result<()> {
        output(std::io::stderr().lock(), args)
    }

    pub fn warn(args: Rest<Value>) -> rquickjs::Result<()> {
        error(args)
    }
}

#[bind(object)]
#[quickjs(rename = "WebAssembly")]
mod web_assembly {
    use super::*;

    #[quickjs(has_refs, cloneable)]
    #[derive(HasRefs, Clone)]
    pub struct InstantiationResultPromiseLike {
        #[quickjs(has_refs)]
        result: InstantiationResult,
    }

    impl InstantiationResultPromiseLike {
        pub fn then<'js>(&self, resolve: Function<'js>) -> rquickjs::Result<Value<'js>> {
            resolve.call((&self.result,))
        }
    }

    #[quickjs(has_refs, cloneable)]
    #[derive(HasRefs, Clone)]
    pub struct InstantiationResult {
        #[quickjs(has_refs)]
        instance: crate::web_assembly::Instance,
    }

    impl InstantiationResult {
        #[quickjs(get)]
        pub fn instance(&self) -> Instance {
            self.instance.clone()
        }
    }

    #[quickjs(has_refs, cloneable)]
    #[derive(HasRefs, Clone)]
    pub struct Instance {
        #[quickjs(has_refs)]
        pub(crate) exports: rquickjs::Persistent<rquickjs::Object<'static>>,
    }

    impl Instance {
        #[quickjs(get)]
        pub fn exports(&self) -> rquickjs::Persistent<rquickjs::Object<'static>> {
            self.exports.clone()
        }
    }

    pub fn instantiate<'js>(
        ctx: Ctx<'js>,
        _module: i32,
        imports: Object<'js>,
    ) -> rquickjs::Result<InstantiationResultPromiseLike> {
        let exports = imports::provide_imports(ctx, imports.get("env")?)?;
        Ok(InstantiationResultPromiseLike {
            result: InstantiationResult {
                instance: Instance {
                    exports: rquickjs::Persistent::save(ctx, exports),
                },
            },
        })
    }

    #[quickjs(cloneable)]
    #[derive(Clone, Copy)]
    pub struct Memory;

    impl Memory {
        #[quickjs(get)]
        pub fn buffer<'js>(&self, ctx: Ctx<'js>) -> Value<'js> {
            unsafe {
                let available_memory = std::arch::wasm32::memory_size::<0>() * 64 * 1024;
                let handle = rquickjs::qjs::JS_NewArrayBuffer(
                    ctx.as_raw().as_ptr(),
                    // Yup, giving it a "slice" of the whole memory from 0-pointer to the end.
                    // What could possibly go wrong? 😅
                    std::ptr::null_mut(),
                    available_memory as u32,
                    None,
                    std::ptr::null_mut(),
                    0,
                );
                Value::from_raw(ctx, handle)
            }
        }
    }

    #[quickjs(has_refs)]
    #[derive(HasRefs)]
    pub struct Table {
        #[quickjs(has_refs)]
        pub(crate) funcs: Vec<Option<rquickjs::Persistent<Function<'static>>>>,
    }

    impl Table {
        pub(crate) fn new<'js>(ctx: Ctx<'js>, funcs: Vec<Option<Function<'js>>>) -> Self {
            Self {
                funcs: funcs
                    .into_iter()
                    .map(|func| Some(rquickjs::Persistent::save(ctx, func?)))
                    .collect(),
            }
        }

        #[quickjs(get)]
        pub fn length(&self) -> usize {
            self.funcs.len()
        }

        pub fn get(&self, index: usize) -> Option<rquickjs::Persistent<Function<'static>>> {
            self.funcs[index].clone()
        }
    }
}

#[bind(object)]
#[quickjs(rename = "Module")]
mod emscripten {
    use super::*;

    #[quickjs(rename = "instantiateWasm")]
    pub fn instantiate_wasm<'js>(
        ctx: Ctx<'js>,
        imports: Object<'js>,
        callback: Function,
    ) -> rquickjs::Result<()> {
        let exports = imports::provide_imports(ctx, imports.get("env")?)?;
        let instance = web_assembly::Instance {
            exports: rquickjs::Persistent::save(ctx, exports),
        };
        callback.call((instance,))
    }
}

thread_local! {
    // Workaround for rquickjs::Context not using a recursive Mutex, so we can't get Ctx again when Wasm calls back into JS via emqjs_invoke_import.
    // Dangerous because it erases the lifetime and only safe to access via `with_active_ctx` that scopes lifetimes back.
    static ACTIVE_CTX: Cell<Option<std::ptr::NonNull<rquickjs::qjs::JSContext>>> = Cell::new(None);
}

pub(crate) fn with_active_ctx<'js, T>(f: impl FnOnce(Ctx<'js>) -> T) -> T {
    ACTIVE_CTX.with(|active_ctx| {
        let active_ctx = active_ctx
            .get()
            .expect("tried to call into JS outside of active context");

        // scope lifetime back
        let active_ctx = unsafe { Ctx::<'js>::from_raw(active_ctx) };

        f(active_ctx)
    })
}

fn start() -> anyhow::Result<()> {
    // tracing_subscriber::fmt::init();

    let runtime = Runtime::new()?;
    let context = Context::full(&runtime)?;
    context.with(|ctx| -> rquickjs::Result<_> {
        ctx.globals().init_def::<Console>()?;
        ctx.globals().init_def::<WebAssembly>()?;
        ctx.globals().init_def::<Emscripten>()?;
        Ok(())
    })?;

    context
        .with(|ctx| {
            ACTIVE_CTX.with(|active_ctx| {
                // println!("Setting active context: {ctx:?}", ctx = ctx.as_ptr());
                assert!(
                    active_ctx.get().is_none(),
                    "tried to set active context while another one is active"
                );
                active_ctx.set(Some(ctx.as_raw()));
                let js_bytes = unsafe {
                    let mut bytes = Vec::with_capacity(externs::js_len());
                    externs::js(bytes.as_mut_ptr());
                    bytes.set_len(externs::js_len());
                    bytes
                };
                let result: rquickjs::Result<()> = ctx.eval(js_bytes);
                // println!("Unsetting active context");
                active_ctx.set(None);
                result.catch(ctx).map_err(|e| e.to_string())
            })
        })
        .map_err(anyhow::Error::msg)?;

    while runtime
        .execute_pending_job()
        .map_err(|e| anyhow::Error::msg(e.to_string()))?
    {}

    Ok(())
}

#[no_mangle]
pub fn emqjs_start() {
    // Since we start in the JS land, change the stack pointer to alternative stack right away.
    // This is to ensure that Emscripten functions to manipulate the stack refer to the original pointer as they
    // will be called on the Wasm side and will get stack swapped for them.
    unsafe {
        externs::swap_stack();
    }

    start().unwrap();
}
