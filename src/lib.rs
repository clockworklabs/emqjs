//! Shared data structure for Wasm preprocessor and the runtime.

use rkyv::{Archive, Serialize};

#[derive(Archive, Serialize, Clone, Copy)]
#[archive(as = "ValueKind")]
pub enum ValueKind {
    I32,
    I64,
    F32,
    F64,
}

#[derive(Archive, Serialize)]
pub struct Func {
    pub name: String,
    pub params: Vec<ValueKind>,
    pub result: Option<ValueKind>,
}

#[derive(Default, Archive, Serialize)]
pub struct Module {
    pub imports: Vec<Func>,
    pub exports: Vec<Func>,
}
