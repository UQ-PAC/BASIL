int x[3]; // is the length of x given as part of its type?
int y;
int z;

// req: y == 0
// ens: 0 <= x[0] + x[1] + x[2] <= 3 * z
// rely: (0 <= y <= z ==> 0 <= y' <= z) && x' == x
void something() {
	int i = 0;
	while (i < 3) {
		x[i] = y;
	}
}

// req: y == 0
// rely: y' == y
void something_else() {
	int i = 0;
	while (i < z) {
		y++;
	}
}