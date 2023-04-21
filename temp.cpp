#include <emscripten.h>
#include <stdio.h>
#include <stdexcept>

EMSCRIPTEN_KEEPALIVE
void foobar() {
  try {
    printf("Throwing...\n");
    throw std::runtime_error("foobar");
    printf("Throwed\n");
  } catch (const std::exception& e) {
    printf("Caught exception: %s\n", e.what());
  }
}

int main() {
  printf("Hello, world!\n");
  foobar();
}
