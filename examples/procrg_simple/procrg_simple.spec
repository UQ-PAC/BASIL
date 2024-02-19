

Globals:
x: int
lock: int


Rely: (lock == 2bv32) ==> ((x == old(x) && (lock == old(lock))))


DIRECT functions: gamma_load64, gamma_load8, memory_load8_le, bvult64, bvule64, bvsub64, gamma_load32, bvuge64, bvugt64
  
Subroutine: main
Requires: x == 0bv32

Subroutine: setb
Modifies: x, lock
Requires: R0 != R1  
Ensures: memory_load(R1, 32) == 10bv32
Rely: (memory_load(old(R0), 32) == 2bv32) ==> ((memory_load(R1, 32)) == old((memory_load(R1, 32))))
Guarantee: DIRECT :: "(forall i : bv64 :: i != old(R1) ==> (memory_load32_le(mem, i) == old(memory_load32_le(mem, i))))" : bool

Subroutine: seta
Modifies: x 
Requires: R0 != R1
Ensures: memory_load(R1, 32) == 10bv32
Ensures: memory_load(R0, 32) == 0bv32
Rely: (memory_load(R0, 32) == 2bv32) ==> ((memory_load(R1, 32)) == old((memory_load(R1, 32))))
Guarantee: DIRECT :: "(forall i : bv64 :: i != old(R1) ==> (memory_load32_le(mem, i) == old(memory_load32_le(mem, i))))" : bool


Subroutine: main
Ensures: x == 10bv32
