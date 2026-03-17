int x;
int z;

// Requires x == 0 && z == 0
// Ensures true
int read() {
	int r1, r2;
	do {
		do {
			r1 = z;
		}
		while (r1 % 2 != 0);
		r2 = x;
	}
	while (z != r1);
	return r2;
}
