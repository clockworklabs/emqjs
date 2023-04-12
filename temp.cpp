#include <emscripten.h>
#include <stdio.h>

EMSCRIPTEN_KEEPALIVE
int foobar() {
	return printf("Hello, world!\n");
}

int main() {
	return foobar();
}
