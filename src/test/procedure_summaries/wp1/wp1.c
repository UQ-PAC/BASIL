/* We require variables in branch conditions to be low (with the analysis). We
 * make x low, and y (possibly) high, then show that if a > 0, then the gamma
 * of c does not matter.
 */

long x;
long y;

__attribute((noinline)) long branch(long a, long b, long c) {
    if (a > 0) {
        if (b > 0) {
            return a;
        } else {
            return b;
        }
    } else {
        if (c > 0) {
            return c;
        } else {
            return a;
        }
    }
}

int main(int argc, char **argv) {
    x = branch(1, x, y);
    return x;
}
