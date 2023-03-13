L: z -> true, x -> z mod 2 == 0, secret -> false
Rely: z == old(z), old(Gamma_x) ==> Gamma_x
Guarantee: z >= old(z)

Subroutine: main
Requires: Gamma_x == true
Requires: Gamma_z == true
Requires: Gamma_secret == false
Requires: z == 0