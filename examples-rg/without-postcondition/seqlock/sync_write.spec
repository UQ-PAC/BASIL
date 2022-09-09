L: z -> true, x -> z % 2 == 0, secret -> false
Rely: z == old(z) && (old(Gamma_x) ==> Gamma_x)
Guarantee: z >= old(z)