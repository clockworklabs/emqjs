//! Shared data structure for Wasm preprocessor and the runtime.

use rkyv::{Archive, Serialize};

#[derive(Archive, Serialize, Clone, Copy)]
#[archive(as = "ValueKind")] // avoid creating separate representation anum for achiving
#[allow(dead_code)] // Rust thinks those variants aren't constructed but they're by rkyv
pub enum ValueKind {
    I32,
    I64,
    F32,
    F64,
}

#[derive(Clone, Archive, Serialize)]
pub struct FuncType {
    pub params: Vec<ValueKind>,
    pub result: Option<ValueKind>,
}

#[derive(Archive, Serialize)]
pub struct Func {
    pub name: String,
    pub ty: FuncType,
}

#[derive(Archive, Serialize)]
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

#[derive(Default, Archive, Serialize)]
pub struct Module {
    pub imports: Vec<Func>,
    pub exports: Vec<Export>,
}

pub const EMQJS_JS_LEN: usize = 1_048_576;
pub const EMQJS_ENCODED_MODULE_LEN: usize = 102_400;
pub const EMQJS_VALUE_SPACE_LEN: usize = 1_024;
