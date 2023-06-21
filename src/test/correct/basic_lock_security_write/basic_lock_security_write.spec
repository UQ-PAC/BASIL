Globals:
x: int
z: int

L: z -> true, x -> z == 0bv32
Rely: z == old(z) && x == old(x)
Guarantee: old(z) == 0bv32 ==> x == old(x) && z == old(z)

Subroutine: main
Requires: z != 0bv32
Requires: Gamma_main_argc == false