#include <stdlib.h>

int x;
int z;

// Requires x == 0 && z == 0
// Ensures true
void sync_write(int secret) {
    int r = 0;
    int y = 0;
    if (!cs(&z, &r, 2)) {
        y = x;
        z = 0;
        return y;
    }
}
