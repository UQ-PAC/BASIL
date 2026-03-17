Globals: 
z: bv32
x: bv32
secret: bv32

L: z -> true, x -> z != 1bv32, secret -> false
Rely: old(Gamma_x) ==> Gamma_x, old(z) == 1bv32 ==> z == old(z)
Guarantee: old(z) == 2bv32 ==> old(z) == z && old(x) == x