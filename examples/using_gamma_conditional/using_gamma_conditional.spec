Globals:
x: int
z: int

L: x -> z == 0, z -> true
Rely: old(Gamma_x) ==> Gamma_x
Guarantee: old(x) == x && old(z) == z

Subroutine: main
Requires: Gamma_x == true