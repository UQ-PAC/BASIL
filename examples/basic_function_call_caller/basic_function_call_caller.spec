Globals:
x: int
y: int

L: x -> true, y -> x == 1
Rely: x == old(x), y == old(y)
Guarantee: old(x) == 0 ==> x == 0, old(Gamma_y) ==> x == 0 || Gamma_y

Subroutine: main
Requires: Gamma_main_argc == false

Subroutine: zero
Ensures: zero_result == 0 && Gamma_zero_result