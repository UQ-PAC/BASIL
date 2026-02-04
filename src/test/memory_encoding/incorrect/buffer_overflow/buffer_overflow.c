#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
  char *buf = malloc(10);

  buf[0] = 'a';

  buf[11] = 'a';
  *(buf - 1) = 'a';
}
