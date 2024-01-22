Globals:
text: char[14]
copiedtext: char[14]



// L: z -> true, x -> z mod 2bv32 == 0bv32


Rely: DIRECT :: "(forall i: bv64 :: (old(mem_locked[i] == 1) ==> (mem_locked[i] == old(mem_locked[i])) && (mem[i] == old(mem[i]))))" : bool
Guarantee: DIRECT :: "(forall i: bv64 :: ((mem_locked[i] == 2) ==> (mem[i] == old(mem[i]))))": bool


DIRECT functions: gamma_load64, gamma_load8, memory_load8_le, bvult64, bvule64, bvsub64, gamma_load32, bvuge64, bvugt64
  
Subroutine: #free
  Requires DIRECT: "(forall i : int, j: bv64 :: (malloc_base[i] == R0 && bvuge64(j, R0) && bvult64(j,  malloc_end[i])) ==> Gamma_mem[j])"

Subroutine: main
Requires DIRECT: "(forall i : bv64 :: mem_locked[i] == 0)"
Requires DIRECT: "malloc_count == 0"
Requires DIRECT: "R31 == 100000bv64"

Subroutine: malloc
Modifies: R0, malloc_base, malloc_count, malloc_end
Requires DIRECT: "bvugt64(R0, 0bv64)"
Requires DIRECT: "Gamma_R0 == true"
Ensures DIRECT: "Gamma_R0 == true"
Ensures DIRECT: "malloc_count == old(malloc_count) + 1"
Ensures DIRECT: "bvugt64(malloc_end[malloc_count], malloc_base[malloc_count])"
Ensures DIRECT: "R0 == malloc_base[malloc_count]"
Ensures DIRECT: "malloc_end[malloc_count] == bvadd64(R0, old(R0))"
Ensures DIRECT: "(forall i: int :: i != malloc_count ==> bvugt64(malloc_base[malloc_count], malloc_end[i]) || bvult64(malloc_end[malloc_count], malloc_base[i]))"
Ensures DIRECT: "(forall i: int :: i != malloc_count ==> malloc_base[i] == old(malloc_base[i]) && malloc_end[i] == old(malloc_end[i]))"
Ensures DIRECT: "bvuge64(R0, 100000000bv64)"
// uninitialised memory is low (free ensures since not part of modifies)
Ensures DIRECT: "(forall i : bv64 :: (bvuge64(i, R0) && bvult64(i, bvadd64(R0, old(R0)))) ==> (Gamma_mem[i] && gamma_load8(Gamma_mem, i)))"

Subroutine: acquire_lock
Modifies: mem_locked
Requires DIRECT: "bvugt64(R0, 0bv64)"
Requires DIRECT: "bvugt64(R1, R0)"
Requires DIRECT: "forall_interval(mem_locked, R0, R1, 0)"
Ensures: DIRECT :: "(forall i : bv64 :: mem_locked[i] == (if (bvuge64(i, old(R0)) && bvule64(i, old(R1))) then (1) else (old(mem_locked[i]))))" : bool
//Ensures DIRECT:  "forall_interval(mem_locked, old(R0), old(R1), 1)"

Subroutine: mmemcpy
  Modifies: mem
  // no concurrent modification
  // dest
  // Requires: DIRECT :: "(forall i: bv64 :: ((bvule64(R0, i) && bvult64(i,bvadd64(R0, R2))) ==> mem_locked[i] == 1))" : bool

  // source
  // Requires: DIRECT :: "(forall i: bv64 :: ((bvule64(R1, i) && bvult64(i,bvadd64(R1, R2))) ==> mem_locked[i] == 1))" : bool
  Ensures DIRECT: "(forall i: bv64 :: (Gamma_mem[i] == if (bvule64(R0, i) && bvult64(i, bvadd64(R0, R2))) then gamma_load8((Gamma_mem), bvadd64(bvsub64(i, R0), R1)) else old(gamma_load8(Gamma_mem, i))))"
  Ensures DIRECT: "(forall i: bv64 :: (mem[i] == if (bvule64(R0, i) && bvult64(i,bvadd64(R0, R2))) then memory_load8_le((mem), bvadd64(bvsub64(i, R0), R1)) else old(memory_load8_le(mem, i))))"

  // Rely on dest and src not being changed concurrently
  Rely: DIRECT :: "(forall i: bv64 :: ((bvule64(R0, i) && bvult64(i,bvadd64(R0, R2))) || (bvule64(R1, i) && bvult64(i,bvadd64(R1, R2)))) ==> mem[i] == old(mem[i]))" : bool
    // this is provable from the fact the regions are locked by the caller
    // not a precond tho
    // Requires DIRECT: "(forall i: bv64 :: ((bvule64(R0, i) && bvult64(i,bvadd64(R0, R2))) || (bvule64(R1, i) && bvult64(i,bvadd64(R1, R2)))) ==> mem_locked[i])"
  // we dont touch any locks
  Guarantee: DIRECT :: "(forall i : bv64 :: mem_locked[i] == old(mem_locked[i]))" : bool


//Subroutine: strlen
//  Ensures : (forall i: bv64 :: (mem_locked[i] == old(mem_locked[i])))
//  Modifies: R0
//  Ensures DIRECT: "Gamma_R0 == true"
//  Ensures DIRECT: "(forall i: bv64 :: (bvule64(old(R0), i)) && (bvult64(i, bvadd64(old(R0), R0))) ==> mem[i] != 0bv8)"
//  Ensures DIRECT: "(memory_load8_le(mem, bvadd64(old(R0), R0)) == 0bv8)"
//  Ensures DIRECT: "(bvult64(old(R0), bvadd64(bvadd64(old(R0), R0), 1bv64)))"
