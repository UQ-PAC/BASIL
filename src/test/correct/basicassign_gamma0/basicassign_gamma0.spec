Globals:
x: int
secret: int

L: secret -> false, x -> true
Rely: secret == old(secret)

Subroutine: main
Requires: Gamma_secret == true