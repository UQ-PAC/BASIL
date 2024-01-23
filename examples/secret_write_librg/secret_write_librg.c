void secret(int* y);

int z;
int x;

int main(void) {
  z = 0;
  z = z + 1;
  secret(&x);
  x = 0;
  z = z + 1;
}