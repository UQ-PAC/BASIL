// L: x -> true
int x;

// req: x == 0
// ens: x == 1 || x == 2
void inc_1() {
	x := x + 1;
}