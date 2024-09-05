Globals:
i: int
x: int

L: x -> true, i -> true

Subroutine: main
Requires: i == 0bv32
Ensures: (i == 5bv32 || i == 6bv32)
