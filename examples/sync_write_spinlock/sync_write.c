#include <stdatomic.h>

int x = 0;
atomic_int z;
int secret;

int main() {
  int expected = 0;
  while (!atomic_compare_exchange_weak(&z, &expected, 1)) {
    expected = 0;
    while (z != 0) {
    }
  }
  x = secret;
  x = 0;
  z = 0;
  return 1;
}