Globals:
x: int
y: int

L: x -> true, y -> x == 1bv32
Rely: old(x) == 0bv32 ==> x == 0bv32, old(Gamma_y) ==> x == 0bv32 || Gamma_y
Guarantee: x == old(x), y == old(y)