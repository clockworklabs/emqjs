use anyhow::Context;
use emqjs_data_structures::{Func, FuncType, ValueKind};
use walrus::ir::{Block, LoadKind, MemArg, StoreKind, Value};
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

fn convert_func_type(ty: &Type) -> anyhow::Result<FuncType> {
    Ok(FuncType {
        params: ty
            .params()
            .iter()
            .copied()
            .map(convert_type)
            .collect::<anyhow::Result<_>>()?,
        result: match ty.results() {
            [] => None,
            [ty] => Some(convert_type(*ty).unwrap()),
            _ => anyhow::bail!("Multi-value results are not supported"),
        },
    })
}

fn main() -> anyhow::Result<()> {
    let mut module = walrus::Module::from_file("temp.wasm")?;

    let emqjs_value_space_ptr = get_pointer_global(&module, "EMQJS_VALUE_SPACE")?;

    let mut emqjs_module = emqjs_data_structures::Module::default();

    // First memory (usually the only one) is the main.
    let memory = module.memories.iter().next().unwrap().id();

    let emqjs_invoke_import = get_export(&module, "emqjs_invoke_import")
        .and_then(unwrap_enum!(ExportItem::Function))
        .copied()?;

    // Replace env imports with trampolines.
    let mut delete_imports = Vec::new();

    module
        .imports
        .iter_mut()
        .filter_map(|i| match i.kind {
            ImportKind::Function(func_id) if i.module == "env" => {
                Some((i.id(), func_id, i.name.as_str()))
            }
            _ => None,
        })
        .enumerate()
        .try_for_each(
            |(table_index, (import_id, func_id, name))| -> anyhow::Result<()> {
                let func_ty = module.types.get(module.funcs.get(func_id).ty()).clone();
                let converted_func_ty = convert_func_type(&func_ty)?;

                let replacement = ReplacementFunc {
                    table_index: table_index as i32,
                    import_id,
                    func_id,
                };

                let mut new_func =
                    FunctionBuilder::new(&mut module.types, func_ty.params(), func_ty.results());
                let mut new_func_body = new_func.func_body();
                let params = func_ty
                    .params()
                    .iter()
                    .map(|&param_ty| module.locals.add(param_ty))
                    .collect::<Vec<_>>();
                for (i, (&param, &param_ty)) in
                    params.iter().zip(&converted_func_ty.params).enumerate()
                {
                    EmqjsSlot {
                        builder: &mut new_func_body,
                        ty: param_ty,
                        memory,
                        emqjs_value_space_ptr,
                        index: i,
                    }
                    .make_store(|builder| {
                        builder.local_get(param);
                    });
                }
                new_func_body.i32_const(replacement.table_index);
                new_func_body.call(emqjs_invoke_import);
                if let Some(result_ty) = converted_func_ty.result {
                    EmqjsSlot {
                        builder: &mut new_func_body,
                        ty: result_ty,
                        memory,
                        emqjs_value_space_ptr,
                        index: 0,
                    }
                    .make_load();
                }
                let new_func_id = new_func.finish(params, &mut module.funcs);

                // workaround for LocalFunction not providing useful direct constructor:
                // remove the function it inserted and put the body at the original index we want
                let new_func_kind = std::mem::replace(
                    &mut module.funcs.get_mut(new_func_id).kind,
                    FunctionKind::Uninitialized(func_ty.id()),
                );
                module.funcs.delete(new_func_id);

                delete_imports.push(replacement.import_id);
                module.funcs.get_mut(replacement.func_id).kind = new_func_kind;

                emqjs_module.imports.push(Func {
                    name: name.to_string(),
                    ty: converted_func_ty,
                });

                Ok(())
            },
        )?;

    for import_id in delete_imports {
        module.imports.delete(import_id);
    }

    // Add export trampoline
    {
        let mut emqjs_invoke_export = FunctionBuilder::new(&mut module.types, &[ValType::I32], &[]);
        let mut emqjs_invoke_export_body = emqjs_invoke_export.func_body();

        // Create bunch of dangling blocks that for now only convert params-results and call their corresponding function.
        let block_ids = module
            .exports
            .iter()
            .filter_map(|e| match e.item {
                ExportItem::Function(func_id) => Some((e.name.as_str(), func_id)),
                _ => None,
            })
            .map(|(name, func_id)| {
                let mut block = emqjs_invoke_export_body.dangling_instr_seq(None);

                let func_ty = convert_func_type(module.types.get(module.funcs.get(func_id).ty()))?;

                for (i, &param_ty) in func_ty.params.iter().enumerate() {
                    EmqjsSlot {
                        builder: &mut block,
                        ty: param_ty,
                        memory,
                        emqjs_value_space_ptr,
                        index: i,
                    }
                    .make_load();
                }
                let call_func = |block: &mut InstrSeqBuilder| {
                    block.call(func_id);
                };
                match &func_ty.result {
                    None => call_func(&mut block),
                    Some(result_ty) => EmqjsSlot {
                        builder: &mut block,
                        ty: *result_ty,
                        memory,
                        emqjs_value_space_ptr,
                        index: 0,
                    }
                    .make_store(call_func),
                }

                block.return_();

                emqjs_module.exports.push(Func {
                    name: name.to_string(),
                    ty: func_ty,
                });

                Ok(block.id())
            })
            .collect::<anyhow::Result<Box<[_]>>>()?;

        // Create an innermost sequence where we'll put our `br_table` instruction.
        let innermost_block_id = emqjs_invoke_export_body.dangling_instr_seq(None).id();

        // Build up the block tree, starting from the innermost one (that has `br_table`) and
        // wrapping into blocks that are its destinations.
        let mut inner_block_id = innermost_block_id;
        for &block_id in block_ids.iter().rev() {
            // Add each block_id as an actual Block instruction to its parent block.
            emqjs_invoke_export_body.instr_seq(block_id).instr(Block {
                seq: inner_block_id,
            });
            inner_block_id = block_id;
        }

        // Outermost body (function itself) is the default destination for the `br_table`.
        let top_id = emqjs_invoke_export_body.id();

        // Now that we processed all block_ids, we can consume them by adding to the innermost block's
        // `br_table` instruction.
        emqjs_invoke_export_body
            .instr_seq(innermost_block_id)
            .br_table(block_ids, top_id);

        // If we reached here, it means that id didn't match any of the blocks. Trap here.
        emqjs_invoke_export_body.unreachable();

        let emqjs_invoke_export_id = emqjs_invoke_export.finish(vec![], &mut module.funcs);

        module.exports.add(
            "emqjs_invoke_export",
            ExportItem::Function(emqjs_invoke_export_id),
        );
    }

    module.data.add(
        DataKind::Active(ActiveData {
            memory,
            location: ActiveDataLocation::Absolute(get_pointer_global(
                &module,
                "EMQJS_ENCODED_MODULE",
            )? as u32),
        }),
        rkyv::to_bytes::<_, 1024>(&emqjs_module)?.into_vec(),
    );

    module.data.add(
        DataKind::Active(ActiveData {
            memory,
            location: ActiveDataLocation::Absolute(get_pointer_global(&module, "EMQJS_JS")? as u32),
        }),
        std::fs::read("temp.js")?,
    );

    module.emit_wasm_file("temp.out.wasm")?;

    Ok(())
}

struct EmqjsSlot<'instr, 'module> {
    builder: &'instr mut InstrSeqBuilder<'module>,
    ty: ValueKind,
    memory: MemoryId,
    emqjs_value_space_ptr: i32,
    index: usize,
}

impl EmqjsSlot<'_, '_> {
    fn make_load(self) {
        self.builder.i32_const(self.emqjs_value_space_ptr);
        // note: the ABI could instead return f64 here and we would use reinterpret;
        // maybe something to consider in the future
        let load_kind = match self.ty {
            ValueKind::I32 => LoadKind::I32 { atomic: false },
            ValueKind::I64 => LoadKind::I64 { atomic: false },
            ValueKind::F32 => LoadKind::F32,
            ValueKind::F64 => LoadKind::F64,
        };
        self.builder.load(
            self.memory,
            load_kind,
            MemArg {
                align: self.ty.size() as u32,
                offset: self.index as u32 * 8,
            },
        );
    }

    fn make_store(self, make_value: impl FnOnce(&mut InstrSeqBuilder)) {
        self.builder.i32_const(self.emqjs_value_space_ptr);
        make_value(self.builder);
        let store_kind = match self.ty {
            ValueKind::I32 => StoreKind::I32 { atomic: false },
            ValueKind::I64 => StoreKind::I64 { atomic: false },
            ValueKind::F32 => StoreKind::F32,
            ValueKind::F64 => StoreKind::F64,
        };
        self.builder.store(
            self.memory,
            store_kind,
            MemArg {
                align: self.ty.size() as u32,
                offset: self.index as u32 * 8,
            },
        );
    }
}
