Globals:
x: int
z: int


L: z -> true, x -> z == 0
Rely: old(z) == 0 ==> x == old(x) && z == old(z)
Guarantee: z == old(z) && x == old(x)