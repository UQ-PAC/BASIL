L: z -> true, x -> z == 0
Rely: old(z) == 2 ==> z == old(z), old(Gamma_x) ==> Gamma_x
Guarantee: x == old(x), old(z) == 1 ==> z == old(z)