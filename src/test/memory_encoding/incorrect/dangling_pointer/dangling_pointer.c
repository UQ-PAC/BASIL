#include <stdlib.h>
#include <string.h>
#include <stdio.h>

int main() {
  char *buf = malloc(10);
  char *buf2 = buf;

  *buf = 12;

  free(buf2);

  *buf = 34;
  free(buf);
}
