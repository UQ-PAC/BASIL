L: z -> true, x -> true
Rely: z == old(z)
Guarantee: old(z) == 0 ==> x == old(x) && z == old(z)

Subroutine: main
Requires: z == 1