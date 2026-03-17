// similar to mutual_postcondition, but more complex

int x;
int y;

// req: x == 0 && y == 2
// ens: x == 3 && y == 1
void T1() {
	x = x + 1;
	y = y + 1;
}

// req: x == 0 && y == 2
// ens: x == 3 && y == 1
void T2() {
	x = x + 2;
	y = y - 2;
}