#include <stdio.h>

int x, y;

int get_two(char a, int b, long long int c) {
    return a + b + c;
}

int main() {
    x = 1;
    y = get_two('a', 10, 10000000000);
    printf("%d\n", y);
}

