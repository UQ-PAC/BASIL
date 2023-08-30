L: x -> true, y -> true, z -> x != 1

T1:
req: x == 0 && y == 0 && z == 0
ens: true
rely: x' == x && (y != 0 ==> y' == y) && z' == z
guar: y' == y && (x != 0 ==> x' == x) && (y != 0 ==> z' == z)

T2:
req: x == 0 && y == 0 && z == 0
ens: true
rely: y' == y && (x != 0 ==> x' == x) && (y != 0 ==> z' == z) && (r2 == 0 ==> r1' != 0 || x' == 0)
guar: x' == x && (y != 0 ==> y' == y) && z' == z

Invariant:
(r1 == 0 ==> r2' != 0 || y' == 0) && (r2 == 0 ==> r1' != 0 || x' == 0)



T1:
rely: x' == x && (y != 0 ==> y' != 0) && z' == z
guar: (x != 0 ==> x' != 0) && y' == y && (y != 0 ==> z' == z || x == 1)

T2:
rely: (x != 0 ==> x' != 0) && y' == y && (y != 0 ==> z' == z || x == 1)
guar: x' == x && (y != 0 ==> y' != 0) && z' == z


z' != z ==> x != 0
z' == z || x != 0
x == 0 ==> z' == z

auxillery variable:
locked ==> z' == z
using local vars:
x == 0 || (x == 1 && y == 1 && r1 == 1) ==> z' == z
restraints over start-stop states:
x == 0 && x' == 1 ==> y == 1 ==> z' == z
x == 1 && y == 0 && y' == 1

***
verifiability depends on knowing that when r1 := y, x == 1
this way, we know that when r1 is assigned 0, the other thread has not assigned r2 to x yet, and hence when it does, r2 will be equal to 1

its clear that we either need auxillery variables to prove this case, or somehow put local vars in the RG conditions whilst always enabling stableR to simplify these variables out

when x := 1 and r1 := y are swapped, we may have:
r1 := y;
	y := 1;
	r2 := x;
x := 1;
hence both r1 == 0 and r2 == 0






// z doesn't change as long as r1 == 1
r1 == 1 ==> z' == z

// if r1 is initially 0, it will remain 0 as long as 

// the only way to get r1' == 1 is if it was already 1 or if it was 0 and y == 1 and it wasn't assigned yet

r1 == 0 ==> r1' == 0