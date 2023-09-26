#include <stdlib.h>
#include <string.h>

#define MALLOC_SIZE 10

int arr[MALLOC_SIZE];

void myfree(void *arg) {
    // require Gamma_myfree_arg[0..10] == true
    free(arg);
}

char *get_secret() {
    // ensure Gamma_get_secret_result[0..10] == false  
    char *secret = (char *)malloc(MALLOC_SIZE);
    return secret;
}

int main() {
    char *secret = get_secret();

    //arr[0] = secret[0];
    memset(secret, 0, MALLOC_SIZE);
    myfree(secret);
}

