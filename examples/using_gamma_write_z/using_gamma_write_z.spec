Globals:
x: int
z: int

L: x -> z == 0, z -> true
Rely: old(x) == x && old(z) == z
Guarantee: old(Gamma_x) ==> Gamma_x

Subroutine: main
Requires: z == 0 ==> Gamma_x