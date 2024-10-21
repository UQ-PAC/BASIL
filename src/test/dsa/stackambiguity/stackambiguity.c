int test(int a, int b, int c, int d) {
  int q = 0;
  int x[4] = {a, b, c};
  int* y = &x[2]; 
  if (d < 3) {
    y = &x[d];
  }
  q = *y + 1;
  return q;
}

int main(int argc, char** argv) {
  int a = 4 + argc;
  int b = 5 * argc;
  int c = 6 - argc;
  int d = test(a, b, c, argc);
  int e = test(b, c, 90, argc);
  return d + e;
}