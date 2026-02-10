int callee(int a, int b) {
  int c = a - b;
  return c;
}

int recursive(int a, int b) {
  int c = callee(a, b);
  if (c > 0) {
    return recursive(a, c);
  } else {
    return c;
  }
}

int main(int argc, char** argv) {
  int d = recursive(0, argc);
  return d;
}