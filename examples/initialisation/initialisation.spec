Globals:
x: bv32
y: bv8
z: bv64
a: bv32[2]

Subroutine: main
Ensures: x == 6bv32
Ensures: a[1] == 4bv32
Ensures: a[0] == 1bv32