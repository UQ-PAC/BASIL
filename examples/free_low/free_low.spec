
DIRECT functions: gamma_load 32

Globals:


Subroutine: main



Subroutine: #free
Requires DIRECT: "gamma_load32(Gamma_mem, R0) == true"

Subroutine: memset
Ensures DIRECT: "gamma_load32(Gamma_mem, R0) == true"
Ensures DIRECT: "Gamma_R0 == true"

Subroutine: get_secret
Ensures DIRECT: "gamma_load32(Gamma_mem, R0) == false"
Ensures DIRECT: "Gamma_R0 == true"




