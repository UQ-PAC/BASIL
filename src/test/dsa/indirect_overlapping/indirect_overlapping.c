#include <stdbool.h>
#include <stdlib.h>

typedef struct {
    long int a;
    long int b;
    long int c;
} str;

int main()
{

    str stack;
    stack.a = 3;
    stack.c = 10;

    str* stackPointer = &stack;

    stackPointer->a = 4;
    stackPointer->b = 12;
    stackPointer->c = 13;
}