


int seven() {
  return 7;
}

void get_call(int (**out)(void)) {
  *out = seven;
}

int main() {
  int (*func)(void);
  get_call(&func);
  return func();
}
