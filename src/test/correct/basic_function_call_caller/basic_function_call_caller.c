int x;
int y;

// Requires true
// Ensures 0 is returned
int zero() {
    return 0;
}

// Requires Gamma_secret == false
// Ensures true
int main(int secret) {
    x = zero();
    y = secret;
}

