mod data_structures;

use anyhow::Context;
use data_structures::{
    Func, FuncType, Module as EmqjsModule, ValueKind, EMQJS_ENCODED_MODULE_LEN, EMQJS_JS_LEN,
    EMQJS_VALUE_SPACE_LEN,
};
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

        let mut delete_imports = Vec::new();

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

                let replacement = ReplacementFunc {
                    table_index: table_index as i32,
                    import_id,
                    func_id,
                };

                let mut new_func = FunctionBuilder::new(
                    &mut self.module.types,
                    func_ty.params(),
                    func_ty.results(),
                );
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
                new_func_body.i32_const(replacement.table_index);
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

                // workaround for LocalFunction not providing useful direct constructor:
                // remove the function it inserted and put the body at the original index we want
                let new_func_kind = std::mem::replace(
                    &mut self.module.funcs.get_mut(new_func_id).kind,
                    FunctionKind::Uninitialized(func_ty.id()),
                );
                self.module.funcs.delete(new_func_id);

                delete_imports.push(replacement.import_id);
                self.module.funcs.get_mut(replacement.func_id).kind = new_func_kind;

                Ok(Func {
                    name: name.to_string(),
                    ty: converted_func_ty,
                })
            })
            .collect::<anyhow::Result<_>>()?;

        for import_id in delete_imports {
            self.module.imports.delete(import_id);
        }

        Ok(imports)
    }

    /// Create `emqjs_invoke_export` trampoline for stores params-results in `EMQJS_VALUE_SPACE` and invokes the underlying export function.
    ///
    /// Returns list of function descriptors for the JS side.
    fn process_exports(&mut self) -> anyhow::Result<Vec<Func>> {
        let mut emqjs_invoke_export =
            FunctionBuilder::new(&mut self.module.types, &[ValType::I32], &[]);
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
            .filter(|&(name, _)| name != "emqjs_invoke_import" && name != "_start")
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
            .br_table(block_ids.into_boxed_slice(), top_id);

        // If we reached here, it means that id didn't match any of the blocks. Trap here.
        emqjs_invoke_export_body.unreachable();

        let new_func_id = emqjs_invoke_export.finish(vec![], &mut self.module.funcs);

        // Find an import to `emqjs_invoke_export` - we'll want to replace it.
        let func_id = {
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

            self.module.imports.delete(import_id);

            func_id
        };

        // workaround for LocalFunction not providing useful direct constructor:
        // remove the function it inserted and put the body at the original index we want
        let new_func = self.module.funcs.get_mut(new_func_id);
        let new_func_type_id = new_func.ty();
        let new_func_kind = std::mem::replace(
            &mut new_func.kind,
            FunctionKind::Uninitialized(new_func_type_id),
        );
        self.module.funcs.get_mut(func_id).kind = new_func_kind;
        self.module.funcs.delete(new_func_id);

        Ok(exports)
    }

    fn write_to_static_byte_array(
        &mut self,
        name: &'static str,
        bytes: impl Into<Vec<u8>>,
        max_len: usize,
    ) -> anyhow::Result<()> {
        let bytes = bytes.into();

        anyhow::ensure!(bytes.len() <= max_len, "Data for array {name} is too long");

        let ptr = take_pointer_global(&mut self.module, name)?;

        self.module.data.add(
            DataKind::Active(ActiveData {
                memory: self.memory,
                location: ActiveDataLocation::Absolute(ptr as u32),
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

        let simple_func_type = self.module.types.add(&[], &[]);
        let table_id = table.id();

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
                .filter_map(|(&func_id, type_dst)| Some((func_id?, type_dst)))
                .try_for_each(|(func_id, type_dst)| -> anyhow::Result<_> {
                    let func = self.module.funcs.get(func_id);
                    let func_ty = self.module.types.get(func.ty());
                    let converted_ty = convert_func_type(func_ty)?;
                    *type_dst = Some(converted_ty);
                    Ok(())
                })?;
        }

        let mut emqjs_invoke_table =
            FunctionBuilder::new(&mut self.module.types, &[ValType::I32], &[]);
        let mut emqjs_invoke_table_body = emqjs_invoke_table.func_body();

        // Create bunch of dangling blocks that for now only convert params-results and call their corresponding function.
        let block_ids = types
            .iter()
            .enumerate()
            .map(|(i, func_ty)| {
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
                    block.call_indirect(simple_func_type, table_id);
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
            // Add each block_id as an actual Block instruction to its parent block.
            emqjs_invoke_table_body.instr_seq(block_id).instr(Block {
                seq: inner_block_id,
            });
            inner_block_id = block_id;
        }

        // Outermost body (function itself) is the default destination for the `br_table`.
        let top_id = emqjs_invoke_table_body.id();

        // Now that we processed all block_ids, we can consume them by adding to the innermost block's
        // `br_table` instruction.
        emqjs_invoke_table_body
            .instr_seq(innermost_block_id)
            .br_table(block_ids, top_id);

        // If we reached here, it means that id didn't match any of the blocks. Trap here.
        emqjs_invoke_table_body.unreachable();

        let new_func_id = emqjs_invoke_table.finish(vec![], &mut self.module.funcs);

        // Find an import to `emqjs_invoke_table` - we'll want to replace it.
        let func_id = {
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

            self.module.imports.delete(import_id);

            func_id
        };

        // workaround for LocalFunction not providing useful direct constructor:
        // remove the function it inserted and put the body at the original index we want
        let new_func = self.module.funcs.get_mut(new_func_id);
        let new_func_type_id = new_func.ty();
        let new_func_kind = std::mem::replace(
            &mut new_func.kind,
            FunctionKind::Uninitialized(new_func_type_id),
        );
        self.module.funcs.get_mut(func_id).kind = new_func_kind;
        self.module.funcs.delete(new_func_id);

        Ok(types)
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
    };

    // Make sure to process exports first:
    // We don't want an import to `env.emqjs_invoke_export` to be treated like any other import.
    let imports = ctx.process_imports()?;
    let exports = ctx.process_exports()?;
    let table = ctx.process_table()?;

    ctx.write_to_static_byte_array(
        "EMQJS_ENCODED_MODULE",
        rkyv::to_bytes::<_, 1024>(&EmqjsModule {
            imports,
            exports,
            table,
        })?,
        EMQJS_ENCODED_MODULE_LEN,
    )?;

    ctx.write_to_static_byte_array("EMQJS_JS", std::fs::read("temp.js")?, EMQJS_JS_LEN)?;

    ctx.module.emit_wasm_file("temp.out.wasm")?;

    Ok(())
}
