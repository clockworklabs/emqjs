#include <emscripten.h>
#include <stdio.h>
#include <stdexcept>

EMSCRIPTEN_KEEPALIVE
void foobar() {
  try {
    throw std::runtime_error("foobar");
  } catch (const std::exception& e) {
    printf("Caught exception: %s\n", e.what());
  }
}

EMSCRIPTEN_KEEPALIVE
void _start() {
  printf("Hello, world!");
  foobar();
}
