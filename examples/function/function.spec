Globals:
x: int
y: int

Subroutine: main
Requires: Gamma_x == true
Requires: Gamma_y == true

Subroutine: get_two
Ensures DIRECT: "Gamma_R0 == true"