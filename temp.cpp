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

static int already_started = 0;

EMSCRIPTEN_KEEPALIVE
int main() {
  if (!already_started) {
    already_started = 1;
    emqjs_start();
    return 0;
  }
  printf("Hello, world!");
  foobar();
}
