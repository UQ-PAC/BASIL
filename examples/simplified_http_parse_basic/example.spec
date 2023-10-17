Globals: 
password: char
buf: long
stext: char[11]


DIRECT functions: gamma_load64, gamma_load8, memory_load8_le, bvult64, bvule64, bvsub64, gamma_load32

  
Subroutine: #free
  Requires DIRECT: "gamma_load8(Gamma_mem, bvadd64(R0, 0bv64)) == true";
  Requires DIRECT: "gamma_load8(Gamma_mem, bvadd64(R0, 1bv64)) == true";
  Requires DIRECT: "gamma_load8(Gamma_mem, bvadd64(R0, 2bv64)) == true";
  Requires DIRECT: "gamma_load8(Gamma_mem, bvadd64(R0, 3bv64)) == true";
Ensures DIRECT: "Gamma_R0 == true"


Subroutine: main
Requires: Gamma_password == false
Requires DIRECT: "gamma_load32(Gamma_mem, memory_load64_le(mem, $stext_addr))"
Requires DIRECT: "R31 == 100bv64"

Subroutine: malloc
Modifies: R0
Ensures: buf == old(buf) && password == old(password) && stext==old(stext)
// modifies R0, Gamma_R0;
Ensures DIRECT: "R0 == 990000000bv64"
Ensures DIRECT: "Gamma_R0 == true"


Subroutine: memcpy
  Modifies: mem
  Ensures: buf == old(buf) && password == old(password) && stext==old(stext)
  Ensures DIRECT: "(forall i: bv64 :: (Gamma_mem[i] == if (bvule64(R0, i) && bvult64(i,bvadd64(R0, R2))) then gamma_load8(old(Gamma_mem), bvadd64(bvsub64(i, R0), R1)) else old(gamma_load8(Gamma_mem, i))))"
  Ensures DIRECT: "(forall i: bv64 :: (mem[i] == if (bvule64(R0, i) && bvult64(i,bvadd64(R0, R2))) then memory_load8_le(old(mem), bvadd64(bvsub64(i, R0), R1)) else old(memory_load8_le(mem, i))))"

// forall i <= n, Gamma_mem[R0] low
Subroutine: memset
  Modifies: mem
  Ensures: buf == old(buf) && password == old(password) && stext==old(stext)
  Ensures DIRECT: "(forall i: bv64 :: (Gamma_mem[i] == if (bvule64(R0, i) && bvult64(i,bvadd64(R0, R2))) then Gamma_R1 else old(Gamma_mem[i])))"
  Ensures DIRECT: "(forall i: bv64 :: (mem[i] == if (bvule64(R0, i) && bvult64(i,bvadd64(R0, R2))) then R1[8:0] else old(memory_load8_le(mem, i))))"


Subroutine: strlen
  Modifies: R0
  Requires DIRECT: "(memory_load8_le(mem, R0) == 0bv8) || (memory_load8_le(mem, bvadd64(R0, 1bv64)) == 0bv8)|| (memory_load8_le(mem, bvadd64(R0, 2bv64)) == 0bv8)|| (memory_load8_le(mem, bvadd64(R0, 3bv64)) == 0bv8)"
  Ensures: buf == old(buf) && password == old(password) && stext==old(stext)
  Ensures DIRECT: "Gamma_R0 == true"
  Ensures DIRECT: "(forall i: bv64 :: (bvule64(old(R0), i)) && (bvult64(i, bvadd64(old(R0), R0))) ==> mem[i] != 0bv8)"
  Ensures DIRECT: "(memory_load8_le(mem, bvadd64(old(R0), R0)) == 0bv8)"
  Ensures DIRECT: "(bvule64(old(R0), bvadd64(old(R0), R0)))"

