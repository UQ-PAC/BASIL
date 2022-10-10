L: z -> true, x -> z mod 2 == 0, secret -> false
Gamma: x -> true, z -> true, secret -> false
Init: z -> 0
Rely: z == old(z), old(Gamma_x) ==> Gamma_x
Guarantee: z >= old(z)