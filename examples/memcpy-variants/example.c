#include <string.h>
#include <stdio.h>

char text[14] = "BASIL Verifier";
char copiedtext[14] = {};

int main() {
   memcpy(copiedtext, text, 14);
}
