Globals: 
z: bv64
x: bv64

L: z -> true, x -> z != 1bv64
Rely: (old(z) == 2bv64) ==> (old(z) == z && (old(Gamma_x) ==> Gamma_x))
Guarantee: old(x) == x, old(z) == 1bv64 ==> z == old(z)

Subroutine: main
Requires: Gamma_x == true
Requires: Gamma_z == true
Requires: z == 0bv64