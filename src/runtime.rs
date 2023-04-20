mod data_structures;
mod runtime_imports;

use data_structures::EMQJS_JS_LEN;
use once_cell::sync::OnceCell;
use rquickjs::{bind, Context, Ctx, Function, HasRefs, Object, Rest, Runtime, Value};

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
        pub fn then<'js>(&'js self, resolve: Function<'js>) -> rquickjs::Result<Value> {
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
        pub fn instance(&self) -> &Instance {
            &self.instance
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
        let exports = runtime_imports::provide_imports(ctx, imports.get("env")?)?;
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
        pub fn buffer<'js>(self, ctx: Ctx<'js>) -> Value<'js> {
            unsafe {
                let available_memory = std::arch::wasm32::memory_size::<0>() * 64 * 1024;
                let handle = rquickjs::qjs::JS_NewArrayBuffer(
                    ctx.as_ptr(),
                    // Yup, giving it a "slice" of the whole memory from 0-pointer to the end.
                    // What could possibly go wrong? 😅
                    std::ptr::null_mut(),
                    available_memory as u32,
                    None,
                    std::ptr::null_mut(),
                    0,
                );
                Value::from_js_value(ctx, handle)
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
        let exports = runtime_imports::provide_imports(ctx, imports.get("env")?)?;
        let instance = web_assembly::Instance {
            exports: rquickjs::Persistent::save(ctx, exports),
        };
        callback.call((instance,))
    }
}

pub static CONTEXT: OnceCell<Context> = OnceCell::new();

#[no_mangle]
static EMQJS_JS: Volatile<[u8; EMQJS_JS_LEN]> = Volatile::new([0; EMQJS_JS_LEN]);

fn start() -> anyhow::Result<()> {
    CONTEXT
        .get_or_try_init(|| -> rquickjs::Result<_> {
            let runtime = Runtime::new()?;
            let context = Context::full(&runtime)?;
            context.with(|ctx| -> rquickjs::Result<_> {
                ctx.globals().init_def::<Console>()?;
                ctx.globals().init_def::<WebAssembly>()?;
                ctx.globals().init_def::<Emscripten>()?;
                Ok(())
            })?;
            Ok(context)
        })?
        .with(|ctx| -> rquickjs::Result<()> {
            ctx.eval(&EMQJS_JS[..EMQJS_JS.iter().position(|b| *b == 0).unwrap_or(0)])?;
            Ok(())
        })?;
    Ok(())
}

#[no_mangle]
pub fn emqjs_start() {
    start().unwrap();
}
