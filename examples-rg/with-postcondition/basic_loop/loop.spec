L: z -> true, x -> true
Rely: x == old(x) || (x == 20 && old(x) == 0) || (x == 20 && old(x) <= 10)
Guarantee: x == old(x) || (x <= 10 && old(x) < 10) || (x == 21 && old(x) == 20)