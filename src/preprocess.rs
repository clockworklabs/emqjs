use anyhow::Context;
use emqjs_data_structures::{Func, ValueKind};
use std::fs;
use walrus::ir::{LoadKind, MemArg, StoreKind, Value};
use walrus::*;

struct ReplacementFunc {
    import_id: ImportId,
    func_id: FunctionId,
    table_index: i32,
}

macro_rules! unwrap_enum {
    ($path:path) => {
        |expr| match expr {
            $path(v) => Ok(v),
            _ => anyhow::bail!("Expected {expr:?} to be of type {}", stringify!($path)),
        }
    };
}

fn get_export<'m>(module: &'m Module, name: &'static str) -> anyhow::Result<&'m ExportItem> {
    module
        .exports
        .iter()
        .find(|e| e.name == name)
        .context(format!("Could not find `{name}` export"))
        .map(|e| &e.item)
}

fn get_pointer_global(module: &Module, name: &'static str) -> anyhow::Result<i32> {
    get_export(module, name)
        .and_then(unwrap_enum!(ExportItem::Global))
        .map(|g| &module.globals.get(*g).kind)
        .and_then(unwrap_enum!(GlobalKind::Local))
        .and_then(unwrap_enum!(InitExpr::Value))
        .and_then(unwrap_enum!(Value::I32))
        .copied()
}

fn convert_type(ty: ValType) -> anyhow::Result<ValueKind> {
    Ok(match ty {
        ValType::I32 => ValueKind::I32,
        ValType::I64 => ValueKind::I64,
        ValType::F32 => ValueKind::F32,
        ValType::F64 => ValueKind::F64,
        _ => anyhow::bail!("Unsupported value type {ty:?}"),
    })
}

fn main() -> anyhow::Result<()> {
    let mut module = walrus::Module::from_file("temp.wasm")?;
    let js = std::fs::read("temp.js")?;

    let emqjs_value_space_ptr = get_pointer_global(&module, "EMQJS_VALUE_SPACE")?;

    let mut emqjs_module = emqjs_data_structures::Module::default();

    let replacements = module
        .imports
        .iter_mut()
        .filter_map(|i| match i.kind {
            ImportKind::Function(func_id) if i.module == "env" => {
                Some((i.id(), func_id, i.name.as_str()))
            }
            _ => None,
        })
        .enumerate()
        .map(|(table_index, (import_id, func_id, name))| {
            let func_ty = module.types.get(module.funcs.get(func_id).ty());

            emqjs_module.imports.push(Func {
                name: name.to_string(),
                params: func_ty
                    .params()
                    .iter()
                    .copied()
                    .map(convert_type)
                    .collect::<anyhow::Result<_>>()?,
                result: match func_ty.results() {
                    [] => None,
                    [ty] => Some(convert_type(*ty)?),
                    _ => anyhow::bail!("Multi-value results are not supported"),
                },
            });

            Ok(ReplacementFunc {
                table_index: table_index as i32,
                import_id,
                func_id,
            })
        })
        .collect::<anyhow::Result<Vec<_>>>()?;

    // First memory (usually the only one) is the main.
    let memory = module.memories.iter().next().unwrap().id();

    module.data.add(
        DataKind::Active(ActiveData {
            memory,
            location: ActiveDataLocation::Absolute(
                get_pointer_global(&module, "EMQJS_IMPORTS")? as u32
            ),
        }),
        rkyv::to_bytes::<_, 1024>(&emqjs_module)?.into_vec(),
    );

    module.data.add(
        DataKind::Active(ActiveData {
            memory,
            location: ActiveDataLocation::Absolute(get_pointer_global(&module, "EMQJS_JS")? as u32),
        }),
        js,
    );

    let emqjs_invoke = get_export(&module, "emqjs_invoke")
        .and_then(unwrap_enum!(ExportItem::Function))
        .copied()?;

    for replacement in &replacements {
        let func_ty_id = module.funcs.get(replacement.func_id).ty();
        let func_ty = module.types.get(func_ty_id).clone();

        let mut new_func =
            FunctionBuilder::new(&mut module.types, func_ty.params(), func_ty.results());
        let mut new_func_body = new_func.func_body();
        let params = func_ty
            .params()
            .iter()
            .map(|&param_ty| module.locals.add(param_ty))
            .collect::<Vec<_>>();
        for (i, (&param, &param_ty)) in params.iter().zip(func_ty.params()).enumerate() {
            new_func_body.i32_const(emqjs_value_space_ptr);
            new_func_body.local_get(param);
            let store_kind = match param_ty {
                ValType::I32 => StoreKind::I32 { atomic: false },
                ValType::I64 => StoreKind::I64 { atomic: false },
                ValType::F32 => StoreKind::F32,
                ValType::F64 => StoreKind::F64,
                _ => anyhow::bail!("Unsupported value type {param_ty:?}"),
            };
            new_func_body.store(
                memory,
                store_kind,
                MemArg {
                    align: 8,
                    offset: i as u32 * 8,
                },
            );
        }
        new_func_body.i32_const(replacement.table_index);
        new_func_body.call(emqjs_invoke);
        match func_ty.results() {
            [] => {}
            [ty] => {
                new_func_body.i32_const(emqjs_value_space_ptr);
                // note: the ABI could instead return f64 here and we would use reinterpret;
                // maybe something to consider in the future
                let load_kind = match ty {
                    ValType::I32 => LoadKind::I32 { atomic: false },
                    ValType::I64 => LoadKind::I64 { atomic: false },
                    ValType::F32 => LoadKind::F32,
                    ValType::F64 => LoadKind::F64,
                    _ => anyhow::bail!("Unsupported value type {ty:?}"),
                };
                new_func_body.load(
                    memory,
                    load_kind,
                    MemArg {
                        align: 8,
                        offset: 0,
                    },
                );
            }
            _ => anyhow::bail!("Multi-value results are not supported"),
        }
        let new_func_id = new_func.finish(params, &mut module.funcs);

        // workaround for LocalFunction not providing useful direct constructor:
        // remove the function it inserted and put at the original index we want
        let new_func_kind = std::mem::replace(
            &mut module.funcs.get_mut(new_func_id).kind,
            FunctionKind::Uninitialized(func_ty_id),
        );
        module.funcs.delete(new_func_id);

        module.imports.delete(replacement.import_id);
        module.funcs.get_mut(replacement.func_id).kind = new_func_kind;
    }

    module.emit_wasm_file("temp.out.wasm")?;

    Ok(())
}
