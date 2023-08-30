int x;
int y;

// req: x == 0 && y == 0
// ens: x != 0
void producer() {
	int r = 20;
	while (x == 0) {
		y++;
		r--;
		if (r == 0) {
			x = 1;
		}
	}
}

// req: x == 0 && y == 0
// ens: x != 0 && y == 0
void consumer() {
	while (x == 0 || y > 0) {
		if (y > 0) {
			y--;
		}
	}
}