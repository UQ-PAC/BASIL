int w;
int x;
int y;
int z;

// req: w == 0 && z == 0
// ens: w == 0 && (z == 0 || z == 1)
void T1() {
	assume w == 0;
	assume z == 0;
	w = 1;
	x = 3;
	w = 0;
}

// req: w == 0 && z == 0
// ens: (w == 0 || w == 1) && z == 0
void T2() {
	assume w == 0;
	z++;
	y = x;
	assert x == y;
	z--;
}