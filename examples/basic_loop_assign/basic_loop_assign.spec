L: x -> true
Rely: x == old(x) || (x <= 10 && old(x) < 10) || (x == 21 && old(x) == 20)
Guarantee: x == old(x) || (x == 20 && old(x) == 0) || (x == 20 && old(x) <= 10)

Subroutine: main
Requires: x == 0
Ensures: x == 20 || x == 21