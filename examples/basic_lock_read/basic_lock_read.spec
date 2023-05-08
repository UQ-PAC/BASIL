Globals:
x: int
z: int

L: z -> true, x -> true
Rely: old(z) == 0bv32 ==> x == old(x) && z == old(z)
Guarantee: z == old(z)

Subroutine: main
Ensures: main_result == 0bv32