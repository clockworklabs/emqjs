set -e
cargo build --lib --target wasm32-wasi
emcc temp.cpp -o temp.js target/wasm32-wasi/debug/libemqjs_runtime.a -s WASM_BIGINT -s ENVIRONMENT=shell -s MINIMAL_RUNTIME -fexceptions -g2 -s ALLOW_MEMORY_GROWTH -sAUTO_{JS,NATIVE}_LIBRARIES=0 -s ASSERTIONS=0 \
	-s EXPORTED_FUNCTIONS=@exported_funcs.list /opt/wasi-sdk/share/wasi-sysroot/lib/wasm32-wasi/libc.a \
	-s ERROR_ON_UNDEFINED_SYMBOLS=0 -Wl,--export=EMQJS_VALUE_SPACE,--import-undefined,--export=EMQJS_ENCODED_MODULE,--export=EMQJS_JS
cargo run --bin emqjs_preprocess
wasmtime temp.out.wasm --dir=.
