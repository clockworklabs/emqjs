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

#[derive(Archive, Serialize)]
pub struct FuncType {
    pub params: Vec<ValueKind>,
    pub result: Option<ValueKind>,
}

#[derive(Archive, Serialize)]
pub struct Func {
    pub name: String,
    pub ty: FuncType,
}

#[derive(Default, Archive, Serialize)]
pub struct Module {
    pub imports: Vec<Func>,
    pub exports: Vec<Func>,
}
