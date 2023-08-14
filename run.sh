#!/bin/bash
set -e
WASI_SDK_PATH=/opt/wasi-sdk cargo rustc -p emqjs-runtime --target wasm32-wasi -- -C target-feature=+bulk-memory,+mutable-globals
wasm-ld --whole-archive --relocatable target/wasm32-wasi/debug/libemqjs_runtime.a -o target/wasm32-wasi/debug/emqjs_runtime.o
emcc -o temp.js temp.cpp target/wasm32-wasi/debug/emqjs_runtime.o \
	-s WASM_BIGINT \
	-s ENVIRONMENT=shell \
	-s STANDALONE_WASM \
	-g2 \
	-s ALLOW_MEMORY_GROWTH \
	-sAUTO_{JS,NATIVE}_LIBRARIES=0 \
	-s ASSERTIONS \
	-s EXPORTED_FUNCTIONS=@exported_funcs.list \
	-fexceptions \
	-s WARN_ON_UNDEFINED_SYMBOLS=0 \
	-s STACK_SIZE=1mb
cargo run -p emqjs-preprocess -- temp.wasm temp.out.wasm
wasmtime temp.out.wasm --dir=.
