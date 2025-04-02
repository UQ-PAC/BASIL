

#include <stdio.h>
int x = 0;
volatile int r = 0;
volatile unsigned z;
volatile unsigned y;

int main(int argc, char **argv)  {
  x = argc;
  y = argc;
  z = argc;

  if (x < 0) {
    x = r;
  }

  if (x > 0) {
    x = r;
  }

  if (x < 5) {
    x += r;
  }

  if (x <= 8) {
    x += r;
  }


  if (x >= 100) {
    x += r;
  }


}

