Globals:
x: int
z: int

L: x -> z == 0, z -> true
Rely: old(z) == z
Guarantee: old(z) != 0 ==> z != 0

Subroutine: main
Requires: Gamma_main_argc == false