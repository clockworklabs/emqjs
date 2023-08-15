#!/bin/bash
set -e
WASI_SDK_PATH=/opt/wasi-sdk cargo rustc -p emqjs-runtime --target wasm32-wasi -- -C target-feature=+bulk-memory,+mutable-globals
wasm-ld --whole-archive --relocatable target/wasm32-wasi/debug/libemqjs_runtime.a -o target/wasm32-wasi/debug/emqjs_runtime.o
