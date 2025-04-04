

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


  if (x > 1000) {
    x += r;
  }


  if (x < r) {
    x += r * 10;
  }
  if (x <= r + 100) {
    x += r * r;
  }
  if (x > r + 1000) {
    x += 2 * r + r;
  }
  if (x >= r + 2000) {
    x += r + r;
  }

  if (y < 0) {
    y += 1;
  }

  if (y <= 0) {
    y += 2;
  }

  if (y <= 1000) {
    y += 1;
  }

  if (y >= -1) {
    y += 1;
  }

  if (y >= z) {
    y += z;
  }

  if (y <= z) {
    y += z;
  }
  if (y < z) {
    y += z;
  }
  if (y > z) {
    y += z;
  }


}

