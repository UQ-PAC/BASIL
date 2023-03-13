L: z -> true, x -> true
Rely: old(z) == 0 ==> x == old(x) && z == old(z)
Guarantee: z == old(z)

Subroutine: main
Ensures: main_result == 0