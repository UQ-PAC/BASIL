#include <stdio.h>
#include <stdlib.h>

void non_returning_function() {
        exit(0);
}

void function_calling_non_returning_function(){
        non_returning_function();
        return;
}


int main() {
        function_calling_non_returning_function();
        return 0;
}
