L: z -> true, x -> z == 0
Rely: old(Gamma_x) ==> Gamma_x, old(z) == 1 ==> z == old(z)
Guarantee: old(z) == 2 ==> z == old(z), x == old(x)