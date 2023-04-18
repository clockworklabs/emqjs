//! Shared data structure for Wasm preprocessor and the runtime.

use rkyv::{Archive, Serialize};

#[derive(Archive, Serialize, Clone, Copy)]
#[archive(as = "ValueKind")]
pub enum ValueKind {
    I32 = 1,
    I64 = 2,
    F32 = 3,
    F64 = 4,
}

#[derive(Archive, Serialize)]
pub struct ImportRequest {
    pub name: String,
    pub params: Vec<ValueKind>,
    pub result: Option<ValueKind>,
}

pub type ImportRequests = Vec<ImportRequest>;
