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

extern "C" void emqjs_start();

// When JS initializes, it will call back into `_start` again.
// We need to keep track of that to make sure that we initialise JS on the first start, and invoke actual code on 2nd.
bool already_started = false;

EMSCRIPTEN_KEEPALIVE
extern "C" void actual_start() {
  printf("Hello, world!\n");
  foobar();
}

EMSCRIPTEN_KEEPALIVE
int main() {
  emqjs_start();
}
