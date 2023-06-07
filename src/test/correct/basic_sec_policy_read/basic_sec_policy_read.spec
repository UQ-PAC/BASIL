Globals:
x: int
z: int

L: x -> z == 0bv32, z -> true
Rely: old(z) != 0bv32 ==> z != 0bv32
Guarantee: old(z) == z