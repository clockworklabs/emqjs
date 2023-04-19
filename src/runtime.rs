mod data_structures;
mod runtime_imports;

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
        exports: rquickjs::Persistent<rquickjs::Object<'static>>,
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
}

#[bind(object)]
#[quickjs(rename = "Module")]
mod emscripten {
    #[quickjs(rename = "wasm")]
    pub const WASM: i32 = 42;
}

pub static CONTEXT: OnceCell<Context> = OnceCell::new();

#[no_mangle]
static EMQJS_JS: Volatile<[u8; 1_048_576]> = Volatile::new([0; 1_048_576]);

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
            // ctx.eval(&EMQJS_JS[..EMQJS_JS.iter().position(|b| *b == 0).unwrap_or(0)])?;
            ctx.eval(std::fs::read("temp.js")?)?;
            Ok(())
        })?;
    Ok(())
}

#[no_mangle]
fn emqjs_start() {
    start().unwrap();
}
