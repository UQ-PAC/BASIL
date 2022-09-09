int x;
int y;

// req: x == 0 && y == 0
// ens: x <= y
// rely: x' == x && y' >= y
// guar: (x <= y ==> x' <= y') && y' == y
void inc_x() {
	int i = 0;
	while (i < 50) {
		// invariant x <= y
		if (x < y) {
			x++;
		}
	}
}

// req: x == 0 && y == 0
// ens: true
// rely: (x <= y ==> x' <= y') && y' == y
// guar: x' == x && y' >= y
void inc_y() {
	int i = 0;
	while (i < 50) {
		if (y < 10) {
			y := y + 3;
		}
	}
}