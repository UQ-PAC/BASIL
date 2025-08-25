// This test should have a `while (a < b) {` loop, but for it to verify we
// would need a loop invariant stating that the gamma of a stays low. We do
// not currently have support for this

__attribute((noinline)) long bound(long a, long b) {
    a = 0;
    while (1) {
    //while (a < b) {
        a++;
        b--;
    }
    return a;
}

int main(int argc, char **argv) {
    return bound(0, 100);
}

