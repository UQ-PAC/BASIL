Globals:
x: int
y: int

L: x -> true, y -> x == 1bv32
Rely: x == old(x), y == old(y)
Guarantee: old(x) == 0bv32 ==> x == 0bv32, old(Gamma_y) ==> x == 0bv32 || Gamma_y

Subroutine: main
Requires: Gamma_main_argc == false

Subroutine: zero
Ensures: zero_result == 0bv32 && Gamma_zero_result