Globals:
x: int
z: int

L: z -> true, x -> z mod 2bv32 == 0bv32
Rely: (z mod 2bv32 == 0bv32 ==> old(Gamma_x) ==> Gamma_x) && z == old(z)
Guarantee: z >= old(z)

Subroutine: main
Requires: Gamma_x == true
Requires: Gamma_z == true
Requires: z == 0bv32

Subroutine: secret
Requires: z mod 2bv32 == 1bv32
Ensures: !Gamma_x && (z mod 2bv32 == 1bv32)
Rely: (z mod 2bv32 == 0bv32 ==> old(Gamma_x) ==> Gamma_x) 
Guarantee: (old(Gamma_x) == Gamma_x)
