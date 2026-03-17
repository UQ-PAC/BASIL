L: x -> true
Rely: old(x) == 5 || old(x) == 6 ==> x == 5 || x == 6
Guarantee: x == old(x) || x == 5