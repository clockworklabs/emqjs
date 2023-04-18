#![allow(unused_imports)]

mod imports;

use once_cell::sync::OnceCell;
use rquickjs::{bind, Context, Ctx, Function, HasRefs, IntoJs, Object, Rest, Runtime, Value};
use std::collections::HashMap;
use std::sync::Arc;

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

    #[quickjs(has_refs)]
    #[derive(HasRefs, Clone)]
    pub struct InstantiationResult {
        #[quickjs(has_refs)]
        instance: Arc<Instance>,
    }

    #[quickjs(has_refs)]
    #[derive(HasRefs)]
    pub struct InstantiationResultPromiseLike {
        #[quickjs(has_refs)]
        result: InstantiationResult,
    }

    impl InstantiationResultPromiseLike {
        pub fn then<'js>(&'js self, resolve: Function<'js>) -> rquickjs::Result<Value> {
            resolve.call((self.result.clone(),))
        }
    }

    #[quickjs(has_refs)]
    #[derive(HasRefs)]
    pub struct Instance {
        #[quickjs(has_refs)]
        exports: HashMap<String, Function<'static>>,
    }

    pub fn instantiate<'js>(
        ctx: Ctx<'js>,
        _module: &other::Module,
        imports: Object<'js>,
    ) -> rquickjs::Result<InstantiationResultPromiseLike> {
        imports::provide_imports(ctx, imports.get("env")?)?;
        let mut exports = HashMap::new();
        Ok(InstantiationResultPromiseLike {
            result: InstantiationResult {
                instance: Arc::new(Instance { exports }),
            },
        })
    }
}

#[bind(object)]
#[quickjs(bare)]
mod other {
    use super::*;

    #[derive(PartialEq)]
    pub struct Module;

    impl Module {
        // We only need a single instance of this.
        #[quickjs(rename = "wasm")]
        pub const WASM: Self = Self;
    }
}

pub static CONTEXT: OnceCell<Context> = OnceCell::new();

fn main() -> anyhow::Result<()> {
    CONTEXT
        .get_or_try_init(|| {
            let runtime = Runtime::new()?;
            Context::full(&runtime)
        })?
        .with(|ctx| -> rquickjs::Result<()> {
            ctx.globals().init_def::<Console>()?;
            ctx.globals().init_def::<WebAssembly>()?;
            ctx.eval("WebAssembly.compile()")?;
            Ok(())
        })?;
    Ok(())
}
