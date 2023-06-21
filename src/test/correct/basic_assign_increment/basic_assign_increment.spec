Globals:
x: int

L: x -> true
Rely: x == old(x) || x == 5bv32
Guarantee: x == old(x) || x == 1bv32 || x == 6bv32

Subroutine: main
Requires: x == 0bv32
Ensures: x == 1bv32 || x == 5bv32 || x == 6bv32