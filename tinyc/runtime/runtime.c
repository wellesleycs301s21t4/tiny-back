/*
 * Runtime support for tiny calculator compiler.
 */
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>

int64_t tiny_input() {
  int64_t x;
  printf("Input: ");
  fflush(stdout);
  scanf("%"PRIi64, &x);
  return x;
}

void tiny_print(int64_t x) {
  printf("Output: %"PRIi64"\n", x);
}
