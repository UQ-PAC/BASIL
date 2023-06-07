Globals:
arr: int[2]

L: arr[0] -> false, arr[1] -> false
Rely: true
Guarantee: old(arr[0]) == arr[0]

Subroutine: main
Requires: Gamma_main_argc == false