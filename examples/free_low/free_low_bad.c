#include <stdlib.h>
#include <string.h>

#define MALLOC_SIZE 10

char *secret;

char *get_secret() {
    // ensure Gamma_get_secret_result[0..10] == false  
    return (char *)malloc(MALLOC_SIZE);
}

int main() {
    secret = get_secret();

    //arr[0] = secret[0];
    //memset(secret, 0, MALLOC_SIZE);
    free(secret); 
}
