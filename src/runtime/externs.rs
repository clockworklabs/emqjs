#[link(wasm_import_module = "emqjs")]
extern "C" {
    pub fn js_len() -> usize;
    /// serialized JS code
    pub fn js(dest: *mut u8);

    pub fn encoded_module_len() -> usize;
    /// encoded Vec<ImportRequest>
    pub fn encoded_module(dest: *mut u8);

    pub fn invoke_export(index: usize);
    pub fn invoke_table(index: usize);

    pub fn swap_stack();
}
