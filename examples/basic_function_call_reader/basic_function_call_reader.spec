Globals:
x: int
y: int

L: x -> true, y -> x == 1
Rely: old(x) == 0 ==> x == 0, old(Gamma_y) ==> x == 0 || Gamma_y
Guarantee: x == old(x), y == old(y)