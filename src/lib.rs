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

impl ValueKind {
    pub const fn size(self) -> usize {
        match self {
            ValueKind::I32 | ValueKind::F32 => 4,
            ValueKind::I64 | ValueKind::F64 => 8,
        }
    }
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
