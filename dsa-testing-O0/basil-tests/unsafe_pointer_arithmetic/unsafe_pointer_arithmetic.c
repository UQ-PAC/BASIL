#include <stdlib.h>


int main() {
    int *bar = malloc(5 * sizeof(int));
    *bar = 12;
    void * car = (void*) bar;
    int* foo = (int*) (car + 1);
    int ** tar = malloc(sizeof(int*));
    *tar = foo;
    int *bat = *tar;
}
