Globals: 
password: char
buf: long
stext: char[11]


DIRECT functions: gamma_load64, gamma_load8, memory_load8_le, bvult64, bvule64, bvsub64

  
Subroutine: #free
Requires DIRECT: "gamma_load64(Gamma_mem, R0)"
Ensures DIRECT: "Gamma_R0 == true"


Subroutine: main
Requires: Gamma_password == false


Subroutine: malloc
Modifies: R0
Ensures: buf == old(buf) && password == old(password)
// modifies R0, Gamma_R0;
Ensures DIRECT: "R0 == 990000bv64"
Ensures DIRECT: "Gamma_R0 == true"


Subroutine: memcpy
Modifies: mem
Ensures: buf == old(buf) && password == old(password) && stext==old(stext)
  Ensures DIRECT: "((memory_load64_le(mem, $buf_addr) == old(memory_load64_le(mem, $buf_addr))) && (memory_load8_le(mem, $password_addr) == old(memory_load8_le(mem, $password_addr))))"
  // Works
  //ensures (forall i: bv64 :: {mem[i]} (memory_load8_le(mem, i) == if (bvule64(R0, i) && bvult64(i,bvadd64(R0, R2))) then memory_load8_le(old(mem), bvadd64(bvsub64(i, R0), R1)) else old(memory_load8_le(mem, i))));
  // Faster 
  Ensures DIRECT: "(forall i: bv64 :: (mem[i] == if (bvule64(R0, i) && bvult64(i,bvadd64(R0, R2))) then memory_load8_le(old(mem), bvadd64(bvsub64(i, R0), R1)) else old(memory_load8_le(mem, i))))"
  // Does not verify with removed \0 write
  // ensures (forall i: bv64 :: (memory_load8_le(mem, i) == if (bvule64(R0, i) && bvult64(i,bvadd64(R0, R2))) then memory_load8_le(old(mem), bvadd64(bvsub64(i, R0), R1)) else old(memory_load8_le(mem, i))));

// forall i <= n, Gamma_mem[R0] low
Subroutine: memset
  Modifies: mem
  Ensures DIRECT: "(forall i: bv64 :: (Gamma_mem[i] == if (bvule64(R0, i) && bvult64(i,bvadd64(R0, R2))) then Gamma_R1 else old(Gamma_mem[i])))"
  // Works
  //ensures (forall i: bv64 :: {mem[i]} (memory_load8_le(mem, i) == if (bvule64(R0, i) && bvult64(i,bvadd64(R0, R2))) then R1[8:0] else old(memory_load8_le(mem, i))));
  // Faster 
  Ensures DIRECT: "(forall i: bv64 :: (mem[i] == if (bvule64(R0, i) && bvult64(i,bvadd64(R0, R2))) then R1[8:0] else old(memory_load8_le(mem, i))))"
  // Does not verify with removed \0 write
  // ensures (forall i: bv64 :: (memory_load8_le(mem, i) == if (bvule64(R0, i) && bvult64(i,bvadd64(R0, R2))) then R1[8:0] else old(memory_load8_le(mem, i))));


Subroutine: strlen
  Ensures: buf == old(buf) && password == old(password)
  Ensures DIRECT: "Gamma_R0 == true"
  Ensures DIRECT: "(forall i: bv64 :: (((bvule64(0bv64, i)) && (bvult64(i, R0)) ==> (mem[bvadd64(old(R0), i)] != 0bv8))))"
  Ensures DIRECT: "(mem[bvadd64(old(R0), R0)] == 0bv8)"
  Ensures DIRECT: "(bvule64(old(R0), bvadd64(old(R0), R0)))" // doesnt overflow


