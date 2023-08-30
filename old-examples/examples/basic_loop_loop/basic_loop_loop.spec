Globals:
x: int

L: x -> true
Rely: x == old(x) || (x == 20bv32 && old(x) == 0bv32) || (x == 20bv32 && old(x) <= 10bv32)
Guarantee: x == old(x) || (x <= 10bv32 && old(x) < 10bv32) || (x == 21bv32 && old(x) == 20bv32)

Subroutine: main
Requires: x == 0bv32
Ensures: x == 10bv32 || x == 20bv32 || x == 21bv32