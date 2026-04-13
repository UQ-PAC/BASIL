#include <stdlib.h>
#include <string.h>
#include <stdio.h>

char password = 7;

int main() {
  char *buf = malloc(11);
  buf[10] = 0; // Clear the final byte.

  memset(buf, password, 10);

  memset(buf, 1, strlen(buf));
  free(buf);
}
