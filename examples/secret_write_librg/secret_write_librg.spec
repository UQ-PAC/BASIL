Globals:
x: int
z: int

L: z -> true, x -> (z mod 2bv32 == 0bv32)
// env doesn't reduce security level of x
// env doesn't change the security classification of x
Rely: old(Gamma_x) ==> Gamma_x, z == old(z)
Guarantee: z >= old(z)

Subroutine: main
Requires: Gamma_x == true
Requires: Gamma_z == true
Requires: z == 0bv32

Subroutine: secret
Requires: z mod 2bv32 == 1bv32
Ensures: !Gamma_x && (z mod 2bv32 == 1bv32)
// env doesn't change security classification or level of x
Rely: z mod 2bv32 == 1bv32, z == old(z), Gamma_x  || !Gamma_x
// we don't change the security classification of x
Guarantee: z == old(z)

// Rely: !Gamma_x && (z == old(z)) && (old(Gamma_x) == Gamma_x)
// Guarantee:  !Gamma_x, (old(Gamma_x) == Gamma_x), z == old(z)


// Gf ==> Gc
// transitive Rc \/ Gf 
