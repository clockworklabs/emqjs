//! Shared data structure for Wasm preprocessor and the runtime.

use rkyv::{Archive, Serialize};

#[derive(Debug, Archive, Serialize, Clone, Copy)]
#[archive(as = "ValueKind")] // avoid creating separate representation anum for achiving
#[allow(dead_code)] // Rust thinks those variants aren't constructed but they're by rkyv
pub enum ValueKind {
    I32,
    I64,
    F32,
    F64,
}

#[derive(Debug, Clone, Archive, Serialize)]
#[archive_attr(derive(Debug))]
pub struct FuncType {
    pub params: Vec<ValueKind>,
    pub result: Option<ValueKind>,
}

#[derive(Debug, Archive, Serialize)]
#[archive_attr(derive(Debug))]
pub struct Func {
    pub name: String,
    pub ty: FuncType,
}

#[derive(Debug, Archive, Serialize)]
#[archive_attr(derive(Debug))]
#[allow(dead_code)]
pub enum Export {
    Func(Func),
    Table {
        name: String,
        types: Vec<Option<FuncType>>,
    },
    Memory {
        name: String,
    },
}

#[derive(Debug, Default, Archive, Serialize)]
#[archive_attr(derive(Debug))]
pub struct Module {
    pub imports: Vec<Func>,
    pub exports: Vec<Export>,
}

pub const EMQJS_JS_LEN: usize = 1024 * 1024;
pub const EMQJS_ENCODED_MODULE_LEN: usize = 1024 * 1024;
// The maximum number of parameters to any function is 1000. (c) the spec
pub const EMQJS_VALUE_SPACE_LEN: usize = 1000;
