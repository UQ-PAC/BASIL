Globals:
x: int
y: int

L: x -> true, y -> true
Rely: x == old(x)
Guarantee: y == old(y)

Subroutine: main
Ensures: x == 1bv32