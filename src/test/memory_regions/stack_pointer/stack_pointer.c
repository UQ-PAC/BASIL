int x = 2;
int* y;

int main(int argc, char** argv) {

  // take a stack pointer, store it in memory, load it again
  int a = x;
  y = &a;
  x = *y + 1;

}