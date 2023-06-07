Globals:
x: int
z: int

L: z -> true, x -> z == 0bv32
Rely: old(z) == 0bv32 ==> x == old(x) && z == old(z)
Guarantee: z == old(z) && x == old(x)