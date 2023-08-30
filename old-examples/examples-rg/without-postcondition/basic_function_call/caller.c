int x;
int y;

// Requires true
// Ensures true
void caller(int secret) {
    x = zero();
    y = secret;
}

// Requires true
// Ensures 0 is returned
int zero() {
    return 0;
}
