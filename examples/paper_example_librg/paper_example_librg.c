
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char text[14] = "BASIL Verifier";
char copiedtext[14];

int acquire_lock(void *begin, void *end) {
  // requires begin...end is a contiguous memory interval
  // lock access to the memory region from [begin, end]
  return (int)begin;
}

void mmemcpy(char *a, char *b, int len) {

  for (int i = 0; i < len; i++) {
    a[i] = b[i];
  }

}

int main() {
  acquire_lock(text, text + 14);
  acquire_lock(copiedtext, copiedtext + 14);
  mmemcpy(copiedtext, text, 14);

  puts(copiedtext);
  puts(text);
}




