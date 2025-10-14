#include <stdio.h>
#include <stdlib.h>

int add(int a, int b) {
  return a + b;
}

int main(int argc, char *argv[]) {
  int a = (int)strtol(argv[1], NULL, 10);
  int b = (int)strtol(argv[2], NULL, 10);

  printf("add[%d] + [%d] = [%d]\n", a, b, add(a, b));
  return 0;
}
