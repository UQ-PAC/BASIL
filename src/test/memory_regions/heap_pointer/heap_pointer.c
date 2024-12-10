#include <stdlib.h>
int x = 5;
int y = 8;

int main(int argc, char** argv) {
  int* heap = (int*) malloc(sizeof(int));
  int* heap2 = (int*) malloc(sizeof(int));
  *heap = x + 1;
  *heap2 = y + 2;
  x = *heap;
  y = *heap2;
}