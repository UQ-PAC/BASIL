#include <stdlib.h>

int x;
int z;

// Requires x == 0 && z == 0
// Ensures true
void sync_write(int secret) {
    int r = 0;
    while (cs(&z, &r, 1)) {
        while (z != 0);
    }
    x = secret;
    x = 0;
    z = 0;
}
