L: x -> true
Rely: x == old(x) || x == 5
Guarantee: x == old(x) || x == 1 || x == 6

Subroutine: main
Requires: x == 0
Ensures: x == 1 || x == 5 || x == 6