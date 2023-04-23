set -e
cargo rustc --lib --target wasm32-wasi -- -C target-feature=+bulk-memory
emcc -o temp.js temp.cpp target/wasm32-wasi/debug/libemqjs_runtime.a \
	-s WASM_BIGINT \
	-s ENVIRONMENT=shell \
	-s STANDALONE_WASM \
	-g2 \
	-s ALLOW_MEMORY_GROWTH \
	-sAUTO_{JS,NATIVE}_LIBRARIES=0 \
	-s ASSERTIONS \
	-s EXPORTED_FUNCTIONS=@exported_funcs.list \
	-fexceptions \
	-s ERROR_ON_UNDEFINED_SYMBOLS=0 \
	-s STACK_SIZE=1mb \
	-Wl,--allow-undefined
cargo run --bin emqjs_preprocess
wasmtime temp.out.wasm --dir=.
