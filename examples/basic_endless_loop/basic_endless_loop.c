#include <stdio.h>

int main(int argc) {
    int n = 27;
    
    while (1) {
        if(n % 2 == 0){
            n = n / 2;
        } else{
            n = n * 3 + 1;
        }
    }

    printf("Testing");
}