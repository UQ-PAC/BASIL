int lock = 0;
int x;

void seta(int *lock, int *x) { 
  *x = 10;
}

void setb(int *lock, int *x) { 
  *lock = 2;
  seta(lock, x);
  *lock = 0;
}

int main() {
  setb(&lock, &x);
}
