#include <stdlib.h>
#include <string.h>
#include <stdio.h>

char password = 7;

int main() {
  char *buf = malloc(11);
  buf[10] = 0;

  memset(buf, password, 10);
  memset(buf+5, 0, 1); // Unsafe

  memset(buf, 1, strlen(buf));
  free(buf);
}
