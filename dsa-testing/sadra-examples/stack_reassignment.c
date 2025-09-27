#include <stdio.h>

int main() {

    int a,b;

    int* c;

    a = 1;
    b = 2;

    c = &a;
    printf("%d", *c);

    c = &b;
    printf("%d", *c);
}