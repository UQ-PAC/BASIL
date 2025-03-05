#include <stdatomic.h>

long x = 0;
atomic_long z = 0;

int main() {

  long y;
  long expected = 0;
  if (atomic_compare_exchange_weak(&z, &expected, 2)) {
    y = x;
    z = 0;
    return y;
  }
  return -1;
}