L: x -> true
Rely: x == old(x) || x == 1 || x == 6
Guarantee: x == old(x) || x == 5

Subroutine: main
Requires: x == 0
Ensures: x == 5 || x == 6