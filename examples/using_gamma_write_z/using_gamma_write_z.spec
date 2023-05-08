Globals:
x: int
z: int

L: x -> z == 0bv32, z -> true
Rely: old(x) == x && old(z) == z
Guarantee: old(Gamma_x) ==> Gamma_x

Subroutine: main
Requires: z == 0bv32 ==> Gamma_x