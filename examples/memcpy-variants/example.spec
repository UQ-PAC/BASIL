Globals: 
text: char[14]
copiedtext: char[14]

// verifies

DIRECT functions: gamma_load64, gamma_load8, memory_load8_le, bvult64, bvule64, bvsub64, gamma_load32, bvuge64, bvugt64

  
Subroutine: main
Requires DIRECT: "malloc_count == 0"
Requires DIRECT: "R31 == 100bv64"
// "BASIL Verifier"
// 66, 65, 83, 73, 76, 32, 86, 101, 114, 105, 102, 105, 101, 114
Ensures: (66bv8 == copiedtext[0]) && (65bv8 == copiedtext[1]) && (83bv8 == copiedtext[2]) && (73bv8 == copiedtext[3]) && (76bv8 == copiedtext[4]) && (32bv8 == copiedtext[5]) &&  (86bv8 == copiedtext[6]) &&  (101bv8 == copiedtext[7]) &&  (114bv8 == copiedtext[8]) && (105bv8 == copiedtext[9]) && (102bv8 == copiedtext[10]) && (105bv8 == copiedtext[11]) == (101bv8 == copiedtext[12]) && (114bv8 == copiedtext[13]) 
Ensures: (text[0] == copiedtext[0]) && (text[1] == copiedtext[1]) && text[2] == copiedtext[2] && text[3] == copiedtext[3] && text[4] == copiedtext[4] && text[5] == copiedtext[5] &&  text[6] == copiedtext[6] &&  text[7] == copiedtext[7] &&  text[8] == copiedtext[8] &&  text[9] == copiedtext[9] &&  text[10] == copiedtext[10] &&  text[11] == copiedtext[11] &&  text[12] == copiedtext[12] &&  text[13] == copiedtext[13] 

Subroutine: memcpy
  Modifies: mem
  // don't overlap
  //Requires DIRECT: "bvugt64(R0, bvadd64(R1, R2)) || bvugt64(R1, bvadd64(R0, R2))"
  // don't wrap around
  //Requires DIRECT: "bvugt64(bvadd64(R0, R2), R0) && bvugt64(bvadd64(R1, R2), R1)"

  Ensures DIRECT: "(forall i: bv64 :: (Gamma_mem[i] == if (bvule64(R0, i) && bvult64(i, bvadd64(R0, R2))) then gamma_load8((Gamma_mem), bvadd64(bvsub64(i, R0), R1)) else old(gamma_load8(Gamma_mem, i))))"
  Ensures DIRECT: "(forall i: bv64 :: {mem[i], old(mem[i])} (mem[i] == if (bvule64(R0, i) && bvult64(i,bvadd64(R0, R2))) then memory_load8_le((mem), bvadd64(bvsub64(i, R0), R1)) else old(memory_load8_le(mem, i))))"
  
