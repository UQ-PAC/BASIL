Globals:
x: int
z: int

L: z -> true, x -> true
Rely: z == old(z)
Guarantee: old(z) == 0bv32 ==> x == old(x) && z == old(z)

Subroutine: main
Requires: z == 1bv32