#ifndef HELPER
#define HELPER
#include <stdbool.h>

volatile long unk;

bool __attribute__ ((noinline)) __VERIFIER_nondet_bool() {
  return (bool)unk;
}

int __attribute__ ((noinline)) __VERIFIER_nondet_int() {
  return (int)unk;
}


unsigned long __attribute__ ((noinline)) __VERIFIER_nondet_ulong () {
  return (unsigned long)unk;
}

long __attribute__ ((noinline)) __VERIFIER_nondet_long () {
  return (long)unk;
}

short __attribute__ ((noinline)) __VERIFIER_nondet_short () {
  return (short)unk;
}

unsigned __attribute__ ((noinline)) __VERIFIER_nondet_uint() {
  return (unsigned)unk;
}

unsigned char __attribute__ ((noinline)) __VERIFIER_nondet_uchar() {
  return (unsigned char)unk;
}

char __attribute__ ((noinline)) __VERIFIER_nondet_char() {
  return (char)unk;
}

#define unknown_int() __VERIFIER_nondet_int() 


#endif
