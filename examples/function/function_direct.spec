Globals:
x: int
y: int

DIRECT functions: gamma_load32

Subroutine: main
Requires DIRECT: "gamma_load32(Gamma_mem, $x_addr) == true"
Requires DIRECT: "gamma_load32(Gamma_mem, $y_addr) == true"

Subroutine: get_two
Ensures DIRECT: "Gamma_get_two_result == true"