char z;
char secret;

int main() {
  char x = secret;
  x = 0;
  z = x;
  x = secret;
  z = x; // Leak
}
