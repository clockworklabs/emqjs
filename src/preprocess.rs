mod data_structures;

use anyhow::Context;
use data_structures::{
    Func, FuncType, Module as EmqjsModule, ValueKind, EMQJS_ENCODED_MODULE_LEN, EMQJS_JS_LEN,
    EMQJS_VALUE_SPACE_LEN,
};
use walrus::ir::{dfs_pre_order_mut, Block, LoadKind, MemArg, StoreKind, Value, VisitorMut};
use walrus::*;

macro_rules! unwrap_enum {
    ($path:path) => {
        |expr| match expr {
            $path(v) => Ok(v),
            _ => anyhow::bail!("Expected {expr:?} to be of type {}", stringify!($path)),
        }
    };
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

fn alignment(ty: ValueKind) -> u32 {
    match ty {
        ValueKind::I32 | ValueKind::F32 => 4,
        ValueKind::I64 | ValueKind::F64 => 8,
    }
}

struct PreprocessCtx {
    module: Module,
    emqjs_value_space: EmqjsValueSpace,
    memory: MemoryId,
    func_id_replacements: Vec<(ImportId, FunctionId, FunctionId)>,
}

fn take_export(module: &mut Module, name: &'static str) -> anyhow::Result<ExportItem> {
    let export = module
        .exports
        .iter_mut()
        .find(|e| e.name == name)
        .context(format!("Could not find `{name}` export"))?;

    let export_item = export.item;

    let export_id = export.id();
    module.exports.delete(export_id);

    Ok(export_item)
}

fn take_pointer_global(module: &mut Module, name: &'static str) -> anyhow::Result<i32> {
    take_export(module, name)
        .and_then(unwrap_enum!(ExportItem::Global))
        .map(|g| &module.globals.get(g).kind)
        .and_then(unwrap_enum!(GlobalKind::Local))
        .and_then(unwrap_enum!(InitExpr::Value))
        .and_then(unwrap_enum!(Value::I32))
        .copied()
}

impl PreprocessCtx {
    /// Replace env imports with trampolines to `emqjs_invoke_import` that store params-results in `EMQJS_VALUE_SPACE`.
    ///
    /// Returns list of function descriptors for the JS side.
    fn process_imports(&mut self) -> anyhow::Result<Vec<Func>> {
        let emqjs_invoke_import = take_export(&mut self.module, "emqjs_invoke_import")
            .and_then(unwrap_enum!(ExportItem::Function))?;

        let imports = self
            .module
            .imports
            .iter_mut()
            .filter_map(|i| match i.kind {
                ImportKind::Function(func_id) if i.module == "env" => {
                    Some((i.id(), func_id, i.name.as_str()))
                }
                _ => None,
            })
            .filter(|&(_, _, name)| name != "emqjs_invoke_export" && name != "emqjs_invoke_table")
            .enumerate()
            .map(|(table_index, (import_id, func_id, name))| {
                println!("Convert import {name}");

                let func_ty = self
                    .module
                    .types
                    .get(self.module.funcs.get(func_id).ty())
                    .clone();
                let converted_func_ty = convert_func_type(&func_ty)?;

                let mut new_func = FunctionBuilder::new(
                    &mut self.module.types,
                    func_ty.params(),
                    func_ty.results(),
                );
                new_func.name(format!("emqjs_invoke_import:{name}"));
                let mut new_func_body = new_func.func_body();
                let params = func_ty
                    .params()
                    .iter()
                    .map(|&param_ty| self.module.locals.add(param_ty))
                    .collect::<Vec<_>>();
                anyhow::ensure!(params.len() <= EMQJS_VALUE_SPACE_LEN, "Too many params");
                for (i, (&param, &param_ty)) in
                    params.iter().zip(&converted_func_ty.params).enumerate()
                {
                    EmqjsSlot {
                        space: self.emqjs_value_space,
                        builder: &mut new_func_body,
                        ty: param_ty,
                        index: i,
                    }
                    .make_store(|builder| {
                        builder.local_get(param);
                    });
                }
                new_func_body.i32_const(table_index as i32);
                new_func_body.call(emqjs_invoke_import);
                if let Some(result_ty) = converted_func_ty.result {
                    EmqjsSlot {
                        space: self.emqjs_value_space,
                        builder: &mut new_func_body,
                        ty: result_ty,
                        index: 0,
                    }
                    .make_load();
                }
                let new_func_id = new_func.finish(params, &mut self.module.funcs);

                self.func_id_replacements
                    .push((import_id, func_id, new_func_id));

                Ok(Func {
                    name: name.to_string(),
                    ty: converted_func_ty,
                })
            })
            .collect::<anyhow::Result<_>>()?;

        Ok(imports)
    }

    /// Create `emqjs_invoke_export` trampoline for stores params-results in `EMQJS_VALUE_SPACE` and invokes the underlying export function.
    ///
    /// Returns list of function descriptors for the JS side.
    fn process_func_exports(&mut self) -> anyhow::Result<Vec<Func>> {
        let mut emqjs_invoke_export =
            FunctionBuilder::new(&mut self.module.types, &[ValType::I32], &[]);
        emqjs_invoke_export.name("emqjs_invoke_export".to_owned());
        let mut emqjs_invoke_export_body = emqjs_invoke_export.func_body();

        // Create bunch of dangling blocks that for now only convert params-results and call their corresponding function.
        let mut block_ids = Vec::new();

        let exports = self
            .module
            .exports
            .iter()
            .filter_map(|e| match e.item {
                ExportItem::Function(func_id) => Some((e.name.as_str(), func_id)),
                _ => None,
            })
            .filter(|&(name, _)| name != "emqjs_invoke_import")
            .map(|(name, func_id)| {
                println!("Converting export {name}");

                let mut block = emqjs_invoke_export_body.dangling_instr_seq(None);

                let func_ty =
                    convert_func_type(self.module.types.get(self.module.funcs.get(func_id).ty()))?;

                anyhow::ensure!(
                    func_ty.params.len() <= EMQJS_VALUE_SPACE_LEN,
                    "Too many params"
                );
                for (i, &param_ty) in func_ty.params.iter().enumerate() {
                    EmqjsSlot {
                        space: self.emqjs_value_space,
                        builder: &mut block,
                        ty: param_ty,
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
                        space: self.emqjs_value_space,
                        builder: &mut block,
                        ty: *result_ty,
                        index: 0,
                    }
                    .make_store(call_func),
                }

                block.return_();

                block_ids.push(block.id());

                Ok(Func {
                    name: name.to_string(),
                    ty: func_ty,
                })
            })
            .collect::<anyhow::Result<_>>()?;

        // Create an innermost sequence where we'll put our `br_table` instruction.
        let innermost_block_id = emqjs_invoke_export_body.dangling_instr_seq(None).id();

        // Build up the block tree, starting from the innermost one (that has `br_table`) and
        // wrapping into blocks that are its destinations.
        let mut inner_block_id = innermost_block_id;
        for &block_id in block_ids.iter().rev() {
            // Add each block_id as an actual Block instruction as the first instruction in its parent block.
            emqjs_invoke_export_body.instr_seq(block_id).instr_at(
                0,
                Block {
                    seq: inner_block_id,
                },
            );
            inner_block_id = block_id;
        }

        // Outermost body (function itself) is the default destination for the `br_table`.
        let top_id = emqjs_invoke_export_body.id();

        let param = self.module.locals.add(ValType::I32);

        // Now that we processed all block_ids, we can consume them by adding to the innermost block's
        // `br_table` instruction.
        emqjs_invoke_export_body
            .instr_seq(innermost_block_id)
            .local_get(param)
            .br_table(block_ids.into_boxed_slice(), top_id);

        // Add the outermost block to the body.
        emqjs_invoke_export_body.instr(Block {
            seq: inner_block_id,
        });
        // If we reached here, it means that id didn't match any of the blocks. Trap here.
        emqjs_invoke_export_body.unreachable();

        let new_func_id = emqjs_invoke_export.finish(vec![param], &mut self.module.funcs);

        // Find an import to `emqjs_invoke_export` - we'll want to replace it.
        let import_id = self
            .module
            .imports
            .find("env", "emqjs_invoke_export")
            .context("Could not find import for `env.emqjs_invoke_export`")?;

        // Get its function id and delete the import.
        let func_id = match &self.module.imports.get(import_id).kind {
            ImportKind::Function(func_id) => *func_id,
            _ => anyhow::bail!("`env.emqjs_invoke_export` is imported as non-function"),
        };

        self.func_id_replacements
            .push((import_id, func_id, new_func_id));

        Ok(exports)
    }

    fn write_to_static_byte_array(
        &mut self,
        name: &'static str,
        offset: usize,
        bytes: impl Into<Vec<u8>>,
        max_len: usize,
    ) -> anyhow::Result<()> {
        let bytes = bytes.into();

        anyhow::ensure!(
            offset + bytes.len() <= max_len,
            "Data for array {name} is too long"
        );

        let ptr = take_pointer_global(&mut self.module, name)?;

        self.module.data.add(
            DataKind::Active(ActiveData {
                memory: self.memory,
                location: ActiveDataLocation::Absolute(ptr as u32 + offset as u32),
            }),
            bytes,
        );

        Ok(())
    }

    fn process_table(&mut self) -> anyhow::Result<Vec<Option<FuncType>>> {
        // We're interested in first and hopefully only table.
        let table = self
            .module
            .tables
            .iter()
            .next()
            .context("Could not find table")?;

        anyhow::ensure!(
            table.element_ty == ValType::Funcref,
            "Only function table is supported"
        );

        anyhow::ensure!(
            Some(table.initial) == table.maximum,
            "Resizable tables are not supported"
        );

        let mut types = vec![None; table.initial as usize];
        let mut dests = vec![None; table.initial as usize];

        for &elem_id in &table.elem_segments {
            let elems = self.module.elements.get(elem_id);

            let offset = match elems.kind {
                ElementKind::Active {
                    offset: InitExpr::Value(Value::I32(offset)),
                    ..
                } => offset as usize,
                kind => anyhow::bail!("Unsupported element kind: {:?}", kind),
            };

            elems
                .members
                .iter()
                .zip(&mut types[offset..])
                .zip(&mut dests[offset..])
                .filter_map(|((&func_id, type_dst), func_dst)| Some((func_id?, type_dst, func_dst)))
                .try_for_each(|(func_id, type_dst, func_dst)| -> anyhow::Result<_> {
                    let func = self.module.funcs.get(func_id);
                    let func_ty = self.module.types.get(func.ty());
                    let converted_ty = convert_func_type(func_ty)?;
                    *type_dst = Some(converted_ty);
                    *func_dst = Some(func_id);
                    Ok(())
                })?;
        }

        let mut emqjs_invoke_table =
            FunctionBuilder::new(&mut self.module.types, &[ValType::I32], &[]);
        emqjs_invoke_table.name("emqjs_invoke_table".to_owned());
        let mut emqjs_invoke_table_body = emqjs_invoke_table.func_body();

        // Create bunch of dangling blocks that for now only convert params-results and call their corresponding function.
        let block_ids = types
            .iter()
            .zip(&dests)
            .map(|(func_ty, func_id)| {
                let mut block = emqjs_invoke_table_body.dangling_instr_seq(None);

                let func_ty = match func_ty {
                    Some(func_ty) => func_ty,
                    None => {
                        // This is a hole in the table. Trap here.
                        block.unreachable();
                        return Ok(block.id());
                    }
                };

                anyhow::ensure!(
                    func_ty.params.len() <= EMQJS_VALUE_SPACE_LEN,
                    "Too many params"
                );
                for (i, &param_ty) in func_ty.params.iter().enumerate() {
                    EmqjsSlot {
                        space: self.emqjs_value_space,
                        builder: &mut block,
                        ty: param_ty,
                        index: i,
                    }
                    .make_load();
                }
                let call_func = |block: &mut InstrSeqBuilder| {
                    block.call(func_id.expect("if func_ty is Some, func_id must be Some"));
                };
                match &func_ty.result {
                    None => call_func(&mut block),
                    Some(result_ty) => EmqjsSlot {
                        space: self.emqjs_value_space,
                        builder: &mut block,
                        ty: *result_ty,
                        index: 0,
                    }
                    .make_store(call_func),
                }

                block.return_();

                Ok(block.id())
            })
            .collect::<anyhow::Result<Box<[_]>>>()?;

        // Create an innermost sequence where we'll put our `br_table` instruction.
        let innermost_block_id = emqjs_invoke_table_body.dangling_instr_seq(None).id();

        // Build up the block tree, starting from the innermost one (that has `br_table`) and
        // wrapping into blocks that are its destinations.
        let mut inner_block_id = innermost_block_id;
        for &block_id in block_ids.iter().rev() {
            // Add each block_id as an actual Block instruction as the first instruction in its parent block.
            emqjs_invoke_table_body.instr_seq(block_id).instr_at(
                0,
                Block {
                    seq: inner_block_id,
                },
            );
            inner_block_id = block_id;
        }

        // Outermost body (function itself) is the default destination for the `br_table`.
        let top_id = emqjs_invoke_table_body.id();

        let param = self.module.locals.add(ValType::I32);

        // Now that we processed all block_ids, we can consume them by adding to the innermost block's
        // `br_table` instruction.
        emqjs_invoke_table_body
            .local_get(param)
            .instr_seq(innermost_block_id)
            .br_table(block_ids, top_id);

        // If we reached here, it means that id didn't match any of the blocks. Trap here.
        emqjs_invoke_table_body.unreachable();

        let new_func_id = emqjs_invoke_table.finish(vec![param], &mut self.module.funcs);

        // Find an import to `emqjs_invoke_table` - we'll want to replace it.
        let import_id = self
            .module
            .imports
            .find("env", "emqjs_invoke_table")
            .context("Could not find import for `env.emqjs_invoke_table`")?;

        // Get its function id and delete the import.
        let func_id = match &self.module.imports.get(import_id).kind {
            ImportKind::Function(func_id) => *func_id,
            _ => anyhow::bail!("`env.emqjs_invoke_table` is imported as non-function"),
        };

        self.func_id_replacements
            .push((import_id, func_id, new_func_id));

        Ok(types)
    }

    fn finish_replacements(&mut self) -> anyhow::Result<()> {
        struct ReplacementVisitor<'a> {
            replacements: &'a [(ImportId, FunctionId, FunctionId)],
        }

        impl VisitorMut for ReplacementVisitor<'_> {
            fn visit_function_id_mut(&mut self, function: &mut walrus::FunctionId) {
                for (_, old, new) in self.replacements {
                    if *function == *old {
                        *function = *new;
                        break;
                    }
                }
            }
        }

        let mut visitor = ReplacementVisitor {
            replacements: &self.func_id_replacements,
        };

        for (_, func) in self.module.funcs.iter_local_mut() {
            dfs_pre_order_mut(&mut visitor, func, func.entry_block());
        }

        for &(import_id, from, _) in self.func_id_replacements.iter() {
            self.module.imports.delete(import_id);
            self.module.funcs.delete(from);
        }

        Ok(())
    }
}

#[derive(Clone, Copy)]
struct EmqjsValueSpace {
    memory: MemoryId,
    ptr: i32,
}

struct EmqjsSlot<'instr, 'module> {
    space: EmqjsValueSpace,
    builder: &'instr mut InstrSeqBuilder<'module>,
    ty: ValueKind,
    index: usize,
}

impl EmqjsSlot<'_, '_> {
    fn make_load(self) {
        self.builder.i32_const(self.space.ptr);
        // note: the ABI could instead return f64 here and we would use reinterpret;
        // maybe something to consider in the future
        let load_kind = match self.ty {
            ValueKind::I32 => LoadKind::I32 { atomic: false },
            ValueKind::I64 => LoadKind::I64 { atomic: false },
            ValueKind::F32 => LoadKind::F32,
            ValueKind::F64 => LoadKind::F64,
        };
        self.builder.load(
            self.space.memory,
            load_kind,
            MemArg {
                align: alignment(self.ty),
                offset: self.index as u32 * 8,
            },
        );
    }

    fn make_store(self, make_value: impl FnOnce(&mut InstrSeqBuilder)) {
        self.builder.i32_const(self.space.ptr);
        make_value(self.builder);
        let store_kind = match self.ty {
            ValueKind::I32 => StoreKind::I32 { atomic: false },
            ValueKind::I64 => StoreKind::I64 { atomic: false },
            ValueKind::F32 => StoreKind::F32,
            ValueKind::F64 => StoreKind::F64,
        };
        self.builder.store(
            self.space.memory,
            store_kind,
            MemArg {
                align: alignment(self.ty),
                offset: self.index as u32 * 8,
            },
        );
    }
}

fn main() -> anyhow::Result<()> {
    let mut module = walrus::Module::from_file("temp.wasm")?;

    // First memory (usually the only one) is the main one we want to target.
    let memory = module.memories.iter().next().unwrap().id();

    let emqjs_value_space = EmqjsValueSpace {
        memory,
        ptr: take_pointer_global(&mut module, "EMQJS_VALUE_SPACE")?,
    };

    let mut ctx = PreprocessCtx {
        module,
        emqjs_value_space,
        memory,
        func_id_replacements: Vec::new(),
    };

    let imports = ctx.process_imports()?;

    let func_exports = ctx.process_func_exports()?;
    let mut exports = func_exports
        .into_iter()
        .map(data_structures::Export::Func)
        .collect::<Vec<_>>();
    let table_export = ctx
        .module
        .exports
        .iter()
        .find(|e| matches!(e.item, ExportItem::Table(_)));
    if let Some(e) = table_export {
        let name = e.name.to_owned();
        ctx.module.exports.delete(e.id());
        let types = ctx.process_table()?;
        exports.push(data_structures::Export::Table { name, types });
    }
    let mem_export = ctx
        .module
        .exports
        .iter()
        .find(|e| matches!(e.item, ExportItem::Memory(_)));
    // Note: memory export must not be deleted.
    if let Some(e) = mem_export {
        exports.push(data_structures::Export::Memory {
            name: e.name.to_owned(),
        });
    }

    let emqjs_encoded_module = rkyv::to_bytes::<_, 1024>(&EmqjsModule { imports, exports })?;

    ctx.write_to_static_byte_array(
        "EMQJS_ENCODED_MODULE",
        // rkyv expects archived root to be at the end of the slice
        // so we need to write it at the end of the static array given to us
        EMQJS_ENCODED_MODULE_LEN - emqjs_encoded_module.len(),
        emqjs_encoded_module,
        EMQJS_ENCODED_MODULE_LEN,
    )?;

    ctx.write_to_static_byte_array("EMQJS_JS", 0, std::fs::read("temp.js")?, EMQJS_JS_LEN)?;

    ctx.finish_replacements()?;
    ctx.module.emit_wasm_file("temp.out.wasm")?;

    Ok(())
}
