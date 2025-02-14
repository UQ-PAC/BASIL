Globals:
x: int
z: int

L: x -> z == 0bv32, z -> true
Rely: old(z) == z
Guarantee: old(z) != 0bv32 ==> z != 0bv32

Subroutine: main
Requires: Gamma_R0 == false
