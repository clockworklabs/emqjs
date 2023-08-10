#[path = "../data_structures.rs"]
mod data_structures;

use anyhow::Context;
use data_structures::{
    Func, FuncType, Module as EmqjsModule, ValueKind, EMQJS_ALT_STACK_LEN, EMQJS_VALUE_SPACE_LEN,
};
use indexmap::IndexMap;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::path::Path;
use walrus::ir::{
    dfs_in_order, dfs_pre_order_mut, Block, Call, CallIndirect, Instr, InstrSeq, InstrSeqId,
    LoadKind, MemArg, StoreKind, Value, Visitor, VisitorMut,
};
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

fn convert_result_type(ty: &Type) -> anyhow::Result<Option<ValueKind>> {
    match ty.results() {
        [] => Ok(None),
        [ty] => Ok(Some(convert_type(*ty)?)),
        _ => anyhow::bail!("Multi-value results are not supported"),
    }
}

fn convert_func_type(ty: &Type) -> anyhow::Result<FuncType> {
    anyhow::ensure!(
        ty.params().len() <= EMQJS_VALUE_SPACE_LEN,
        "Too many params"
    );
    Ok(FuncType {
        params: ty
            .params()
            .iter()
            .copied()
            .map(convert_type)
            .collect::<anyhow::Result<_>>()?,
        result: convert_result_type(ty)?,
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
    thrown: GlobalId,
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
                // this will skip WASI imports as well as our injected `emqjs` imports, processing only custom C functions
                // which is what we want to transform
                ImportKind::Function(func_id) if i.module == "env" => {
                    Some((i.id(), func_id, i.name.as_str()))
                }
                _ => None,
            })
            .enumerate()
            .map(|(table_index, (import_id, func_id, name))| {
                tracing::debug!("Converting import {name}");

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

                new_func_body.if_else(
                    None,
                    |_| {},
                    |on_error| {
                        on_error.i32_const(1);
                        on_error.global_set(self.thrown);
                    },
                );

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

    /// Create `emqjs_invoke_export` trampoline for stored params-results in `EMQJS_VALUE_SPACE` and invokes the underlying export function.
    ///
    /// Returns list of function descriptors for the JS side.
    fn process_func_exports(&mut self) -> anyhow::Result<Vec<Func>> {
        let mut emqjs_invoke_export =
            FunctionBuilder::new(&mut self.module.types, &[ValType::I32], &[]);
        emqjs_invoke_export.name("emqjs_invoke_export".to_owned());
        let mut emqjs_invoke_export_body = emqjs_invoke_export.func_body();

        let mut export_func_ids = Vec::new();

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
                tracing::debug!("Converting export {name}");

                export_func_ids.push(func_id);

                Ok(Func {
                    name: name.to_owned(),
                    ty: convert_func_type(
                        self.module.types.get(self.module.funcs.get(func_id).ty()),
                    )?,
                })
            })
            .collect::<anyhow::Result<Vec<_>>>()?;

        let space = self.emqjs_value_space;

        let thrown = self.thrown;

        let block_builders = exports.iter().zip(export_func_ids).map(|(func, func_id)| {
            move |block: &mut InstrSeqBuilder| {
                let call_func = |block: &mut InstrSeqBuilder| {
                    for (i, &param_ty) in func.ty.params.iter().enumerate() {
                        EmqjsSlot {
                            space,
                            builder: block,
                            ty: param_ty,
                            index: i,
                        }
                        .make_load();
                    }
                    block.call(func_id);
                };
                match &func.ty.result {
                    None => call_func(block),
                    Some(result_ty) => EmqjsSlot {
                        space,
                        builder: block,
                        ty: *result_ty,
                        index: 0,
                    }
                    .make_store(call_func),
                }

                // Stop unwinding, it's now responsibility of JS to check for exceptions.
                block.i32_const(0);
                block.global_set(thrown);

                block.return_();
            }
        });

        let discriminant = self.module.locals.add(ValType::I32);

        Self::build_switch(
            discriminant,
            &mut emqjs_invoke_export_body,
            Vec::new(),
            block_builders,
        );

        let new_func_id = emqjs_invoke_export.finish(vec![discriminant], &mut self.module.funcs);

        self.replace_emqjs_import("invoke_export", new_func_id)?;

        Ok(exports)
    }

    fn write_to_static_byte_array(
        &mut self,
        name: &'static str,
        bytes: impl Into<Vec<u8>>,
    ) -> anyhow::Result<()> {
        let bytes = bytes.into();
        let bytes_len = bytes.len() as i32;

        {
            let name = format!("{name}_len");

            let mut builder = FunctionBuilder::new(&mut self.module.types, &[], &[ValType::I32]);
            builder.name(name.clone());
            builder.func_body().i32_const(bytes_len);
            let new_func_id = builder.finish(vec![], &mut self.module.funcs);

            self.replace_emqjs_import(&name, new_func_id)?;
        }

        {
            let mut builder = FunctionBuilder::new(&mut self.module.types, &[ValType::I32], &[]);
            builder.name(name.to_owned());
            let ptr_param_id = self.module.locals.add(ValType::I32);
            let data_id = self.module.data.add(DataKind::Passive, bytes);
            builder
                .func_body()
                .local_get(ptr_param_id)
                .i32_const(0)
                .i32_const(bytes_len)
                .memory_init(self.memory, data_id)
                .data_drop(data_id);
            let new_func_id = builder.finish(vec![ptr_param_id], &mut self.module.funcs);

            self.replace_emqjs_import(name, new_func_id)?;
        }

        Ok(())
    }

    fn replace_emqjs_import(
        &mut self,
        name: &str,
        new_func_id: FunctionId,
    ) -> Result<(), anyhow::Error> {
        let import_id = self
            .module
            .imports
            .find("emqjs", name)
            .with_context(|| format!("Could not find import for `emqjs.{name}`"))?;
        let func_id = match &self.module.imports.get(import_id).kind {
            ImportKind::Function(func_id) => *func_id,
            _ => anyhow::bail!("`env.{name}` is imported as non-function"),
        };
        self.func_id_replacements
            .push((import_id, func_id, new_func_id));
        Ok(())
    }

    fn build_switch<F: FnOnce(&mut InstrSeqBuilder)>(
        discriminant: LocalId,
        parent: &mut InstrSeqBuilder,
        mut block_ids: Vec<InstrSeqId>,
        mut build_blocks: impl Iterator<Item = F>,
    ) {
        match build_blocks.next() {
            Some(build_block) => {
                parent.block(None, |nested| {
                    block_ids.push(nested.id());
                    Self::build_switch(discriminant, nested, block_ids, build_blocks);
                });
                build_block(parent);
            }
            None => {
                parent.block(None, |nested| {
                    nested.local_get(discriminant);
                    nested.br_table(block_ids.into_boxed_slice(), nested.id());
                });
                parent.unreachable();
            }
        }
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

        let mut emqjs_invoke_table =
            FunctionBuilder::new(&mut self.module.types, &[ValType::I32], &[]);
        emqjs_invoke_table.name("emqjs_invoke_table".to_owned());

        let mut types = vec![None; table.initial as usize];

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
                    *type_dst = Some(func_ty);
                    Ok(())
                })?;
        }

        let mut type_blocks = IndexMap::<&Type, InstrSeqId>::new();

        let func_id_param = self.module.locals.add(ValType::I32);

        // Create bunch of dangling blocks that for now only convert params-results and call their corresponding function.
        let space = self.emqjs_value_space;

        let thrown = self.thrown;

        let mut func_body = emqjs_invoke_table.func_body();

        let top_block_id = func_body.dangling_instr_seq(None).id();

        let mut block_id = top_block_id;
        for &func_ty in &types {
            let Some(func_ty) = func_ty else {
                continue;
            };
            let indexmap::map::Entry::Vacant(entry) = type_blocks.entry(func_ty) else {
                continue;
            };

            // the block we'll want to break out from to invoke this type of function
            let nested_id = func_body.dangling_instr_seq(None).id();
            // associate this function type with the nested block for the br_table so it knows to break out into our current `block`
            entry.insert(nested_id);
            // fill the current block with the nested block followed by typed invocation
            // nested blocks all end in `br $top_block` so typed invocation is only reachable via innermost `br_table`
            let block = &mut func_body.instr_seq(block_id);
            block.instr(Block { seq: nested_id });
            let converted_ty = convert_func_type(func_ty)?;
            let call_func = |block: &mut InstrSeqBuilder| {
                for (i, &param_ty) in converted_ty.params.iter().enumerate() {
                    EmqjsSlot {
                        space,
                        builder: block,
                        ty: param_ty,
                        index: i,
                    }
                    .make_load();
                }
                block
                    .local_get(func_id_param)
                    .call_indirect(func_ty.id(), table.id());
            };
            match &converted_ty.result {
                None => call_func(block),
                Some(result_ty) => EmqjsSlot {
                    space,
                    builder: block,
                    ty: *result_ty,
                    index: 0,
                }
                .make_store(call_func),
            }
            // in the end of the current block we want to break out into the top block
            // which does the exception cleanup
            block.br(top_block_id);
            // finally, set current block to the nested one we created
            block_id = nested_id;
        }
        // add block that catches table holes (unknown type) and unknown function ids
        {
            let nested_id = func_body.dangling_instr_seq(None).id();
            func_body
                .instr_seq(block_id)
                .instr(Block { seq: nested_id })
                .unreachable();
            block_id = nested_id;
        }

        // we don't have any more types, fill the innermost block with the `br_table` that jumps to matching type handler based on function id
        // or just outside to the `unreachable` trap if function id is out of bounds or there is no type
        func_body
            .instr_seq(block_id)
            .local_get(func_id_param)
            .br_table(
                types
                    .iter()
                    .map(|ty| ty.map_or(block_id, |ty| type_blocks[ty]))
                    .collect(),
                block_id,
            );

        // finish the function body by inserting the top block & the exception cleanup all blocks jump out to after the calls
        func_body
            .instr(Block { seq: top_block_id })
            // Stop unwinding, it's now responsibility of JS to check for exceptions.
            .i32_const(0)
            .global_set(thrown);

        let new_func_id = emqjs_invoke_table.finish(vec![func_id_param], &mut self.module.funcs);

        let converted_types = types
            .iter()
            .map(|&ty| {
                Some(
                    convert_func_type(ty?)
                        .expect("convert_func_type already fallibly checked above"),
                )
            })
            .collect();

        self.replace_emqjs_import("invoke_table", new_func_id)?;

        Ok(converted_types)
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
        self.module
            .elements
            .iter_mut()
            .flat_map(|e| e.members.iter_mut())
            .flatten()
            .for_each(|f| visitor.visit_function_id_mut(f));

        for &(import_id, from, _) in self.func_id_replacements.iter() {
            self.module.imports.delete(import_id);
            self.module.funcs.delete(from);
        }

        Ok(())
    }

    fn process_traps(&mut self) -> anyhow::Result<()> {
        struct FuncInfo {
            id: FunctionId,
            block_ids: Vec<InstrSeqId>,
        }

        struct AllBlocksVisitor<'t, 'm> {
            might_throw: &'t mut HashMap<FunctionId, bool>,
            funcs: &'m ModuleFunctions,
            imports: &'m ModuleImports,
            func_info: FuncInfo,
            block_might_throw: bool,
            func_might_throw: bool,
        }

        impl AllBlocksVisitor<'_, '_> {
            fn might_throw(&mut self, func_id: FunctionId) -> bool {
                let entry = match self.might_throw.entry(func_id) {
                    Entry::Occupied(in_cache) => {
                        return *in_cache.get();
                    }
                    Entry::Vacant(v) => v,
                };

                let func = match &self.funcs.get(func_id).kind {
                    FunctionKind::Import(imported_func) => {
                        // assume that JS functions might throw.
                        let can_throw = self.imports.get(imported_func.import).module == "env";
                        entry.insert(can_throw);
                        return can_throw;
                    }
                    FunctionKind::Local(local) => {
                        // Insert `false` to break off recursion.
                        entry.insert(false);
                        local
                    }
                    FunctionKind::Uninitialized(_) => unreachable!("uninitialized function"),
                };

                struct TraverseCallsVisitor<'s, 't, 'm> {
                    this: &'s mut AllBlocksVisitor<'t, 'm>,
                    might_throw: bool,
                }

                impl Visitor<'_> for TraverseCallsVisitor<'_, '_, '_> {
                    fn visit_call(&mut self, instr: &Call) {
                        self.might_throw |= self.this.might_throw(instr.func);
                    }

                    fn visit_call_indirect(&mut self, _instr: &CallIndirect) {
                        self.might_throw |= true;
                    }
                }

                let might_throw = {
                    let mut visitor = TraverseCallsVisitor {
                        this: self,
                        might_throw: false,
                    };
                    dfs_in_order(&mut visitor, func, func.entry_block());
                    visitor.might_throw
                };

                self.might_throw.insert(func_id, might_throw);

                might_throw
            }
        }

        impl Visitor<'_> for AllBlocksVisitor<'_, '_> {
            fn start_instr_seq(&mut self, _instr_seq: &InstrSeq) {
                self.block_might_throw = false;
            }

            fn end_instr_seq(&mut self, instr_seq: &InstrSeq) {
                if self.block_might_throw {
                    self.func_might_throw = true;
                    self.func_info.block_ids.push(instr_seq.id());
                }
            }

            fn visit_call(&mut self, instr: &Call) {
                // Note: we visit function regardless of current block status
                // as we want all functions to be in the "might throw" cache.
                self.block_might_throw |= self.might_throw(instr.func);
            }

            fn visit_call_indirect(&mut self, _instr: &CallIndirect) {
                self.block_might_throw = true;
            }

            fn visit_unreachable(&mut self, _instr: &ir::Unreachable) {
                self.block_might_throw = true;
            }
        }

        let mut func_infos = Vec::new();
        let mut might_throw = HashMap::new();

        for (func_id, func) in self.module.funcs.iter_local() {
            let mut visitor = AllBlocksVisitor {
                func_info: FuncInfo {
                    id: func_id,
                    block_ids: Vec::new(),
                },
                block_might_throw: false,
                func_might_throw: false,
                might_throw: &mut might_throw,
                funcs: &self.module.funcs,
                imports: &self.module.imports,
            };

            walrus::ir::dfs_in_order(&mut visitor, func, func.entry_block());

            if visitor.func_might_throw {
                func_infos.push(visitor.func_info);
            }
        }

        for func_info in func_infos {
            let func = self
                .module
                .funcs
                .get_mut(func_info.id)
                .kind
                .unwrap_local_mut();

            let return_type = convert_result_type(self.module.types.get(func.ty()))?;

            let func_builder = func.builder_mut();

            let body_wrapper = func_builder.dangling_instr_seq(None).id();

            for block_id in func_info.block_ids {
                let mut block = func_builder.instr_seq(block_id);

                let mut instrs = std::mem::take(block.instrs_mut()).into_iter().peekable();

                while let Some((instr, instr_loc)) = instrs.next() {
                    let might_throw = match instr {
                        Instr::Call(Call { func }) => might_throw
                            .get(&func)
                            .copied()
                            .expect("all functions should be in the might_throw cache by now"),
                        Instr::CallIndirect(_) => true,
                        _ => false,
                    };

                    block.instrs_mut().push((instr, instr_loc));

                    if might_throw
                        && !matches!(
                            instrs.peek(),
                            // only insert checks if we don't return immediately,
                            // otherwise it's okay to propagate flag upwards
                            None | Some((Instr::Return(_), _))
                        )
                    {
                        // if the next one is `unreachable`, then break off without checking -
                        // the exception is clearly expected
                        if let Some((Instr::Unreachable(_), _)) = instrs.peek() {
                            block.br(body_wrapper);
                        } else {
                            // Otherwise check if the thrown flag got set.
                            block.global_get(self.thrown);
                            block.br_if(body_wrapper);
                        }
                    }
                }
            }

            let body_instrs = std::mem::take(func_builder.func_body().instrs_mut());

            let mut body_wrapper_builder = func_builder.instr_seq(body_wrapper);
            *body_wrapper_builder.instrs_mut() = body_instrs;
            body_wrapper_builder.return_();

            let mut func_body = func_builder.func_body();
            func_body.instr(Instr::Block(Block { seq: body_wrapper }));
            if let Some(return_type) = return_type {
                match return_type {
                    ValueKind::I32 => func_body.i32_const(0),
                    ValueKind::I64 => func_body.i64_const(0),
                    ValueKind::F32 => func_body.f32_const(0.0),
                    ValueKind::F64 => func_body.f64_const(0.0),
                };
            }
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
        // wrap into block just for validation -
        // otherwise it's easy to accidentally use the value from outside or leave something on stack
        self.builder.block(
            match self.ty {
                ValueKind::I32 => ValType::I32,
                ValueKind::I64 => ValType::I64,
                ValueKind::F32 => ValType::F32,
                ValueKind::F64 => ValType::F64,
            },
            make_value,
        );
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

fn threaded_main() -> anyhow::Result<()> {
    let mut args = std::env::args().skip(1);
    let input_wasm = args.next().context("No input wasm file provided")?;
    let input_js = Path::new(&input_wasm).with_extension("js");
    let output_wasm = args.next().context("No output wasm file provided")?;

    let mut module = walrus::Module::from_file(input_wasm)?;

    // First memory (usually the only one) is the main one we want to target.
    let memory = module.memories.iter().next().unwrap().id();

    let emqjs_value_space = EmqjsValueSpace {
        memory,
        ptr: take_pointer_global(&mut module, "EMQJS_VALUE_SPACE")?,
    };

    let mut ctx = PreprocessCtx {
        thrown: module
            .globals
            .add_local(ValType::I32, true, InitExpr::Value(Value::I32(0))),
        module,
        emqjs_value_space,
        memory,
        func_id_replacements: Vec::new(),
    };

    ctx.process_traps()?;

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

    ctx.write_to_static_byte_array("encoded_module", emqjs_encoded_module)?;

    ctx.write_to_static_byte_array("js", std::fs::read(input_js)?)?;

    {
        let emqjs_alt_stack = take_export(&mut ctx.module, "EMQJS_ALT_STACK")
            .and_then(unwrap_enum!(ExportItem::Global))?;
        // Make it mutable as we want to reuse it for stored stack.
        ctx.module.globals.get_mut(emqjs_alt_stack).mutable = true;
        let emqjs_alt_stack_initial_value =
            Ok(&mut ctx.module.globals.get_mut(emqjs_alt_stack).kind)
                .and_then(unwrap_enum!(GlobalKind::Local))
                .and_then(unwrap_enum!(InitExpr::Value))
                .and_then(unwrap_enum!(Value::I32))?;
        *emqjs_alt_stack_initial_value += EMQJS_ALT_STACK_LEN as i32;

        let stack_pointer = take_export(&mut ctx.module, "__stack_pointer")
            .and_then(unwrap_enum!(ExportItem::Global))?;

        let mut new_func = FunctionBuilder::new(&mut ctx.module.types, &[], &[]);
        new_func.name("emqjs_swap_stack".to_owned());

        new_func
            .func_body()
            .global_get(stack_pointer)
            .global_get(emqjs_alt_stack)
            .global_set(stack_pointer)
            .global_set(emqjs_alt_stack);

        let new_func_id = new_func.finish(vec![], &mut ctx.module.funcs);

        ctx.replace_emqjs_import("swap_stack", new_func_id)?;
    }

    // Original _start is preserved in the trampoline now.
    // Replace the Wasm `_start` with `emqjs_start` export that starts the JS runtime instead.

    let emqjs_start_export = ctx
        .module
        .exports
        .iter()
        .find(|e| e.name == "emqjs_start")
        .context("Could not find `emqjs_start` export")?;

    let emqjs_start_item = emqjs_start_export.item;
    ctx.module.exports.delete(emqjs_start_export.id());

    ctx.module
        .exports
        .iter_mut()
        .find(|e| e.name == "_start")
        .context("Could not find `_start` export")?
        .item = emqjs_start_item;

    ctx.finish_replacements()?;
    ctx.module.emit_wasm_file(output_wasm)?;

    Ok(())
}

fn main() -> anyhow::Result<()> {
    std::thread::Builder::new()
        .stack_size(80 * 1024 * 1024)
        .spawn(threaded_main)?
        .join()
        .unwrap()
}
