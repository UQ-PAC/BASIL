#include <stdio.h>
#include <stdlib.h>

void non_returning_function() {
        exit(0);
}

int main() {
        non_returning_function();
        return 0;
}
