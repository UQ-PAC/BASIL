Globals:
password: char
// buf: long
// stext: char[11]

L: password -> false
// Rely: x == old(x) || x == 0bv32
// Guarantee: x == old(x) || x == 5bv32

Subroutine: main
// Requires: x == 0bv32
// Ensures: x == 5bv32 || x == 6bv32
