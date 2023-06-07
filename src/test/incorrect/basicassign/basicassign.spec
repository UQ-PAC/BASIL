Globals:
x: int
secret: int
z: int

L: x -> false, secret -> false, z -> true
Rely: x == old(x)
// designed to not verify