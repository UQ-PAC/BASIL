int x;

void set_two() {
  x = 2;
}

void set_six() {
  x = 6;
}

void set_seven() {
  x = 7;
}
  
int main(int argc) {   
  void (*funcptr)();

  switch (argc) {
    case 0: funcptr = &set_two; break;
    case 1: funcptr = &set_six; break;
    case 2: funcptr = &set_seven; break;
    default: funcptr = &set_two; break;
  }

  (*funcptr)();

  return 0;
}