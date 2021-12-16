package util

object Boogie {
  def generateBVHeader (size: Int) = f"""
      |/*****
      | * Bitvector functions for bv$size
      | ****/
      |// Arithmetic
      |function {:bvbuiltin "bvadd"} bv${size}add(bv${size},bv${size}) returns(bv${size});
      |function {:bvbuiltin "bvsub"} bv${size}sub(bv${size},bv${size}) returns(bv${size});
      |function {:bvbuiltin "bvmul"} bv${size}mul(bv${size},bv${size}) returns(bv${size});
      |function {:bvbuiltin "bvudiv"} bv${size}udiv(bv${size},bv${size}) returns(bv${size});
      |function {:bvbuiltin "bvurem"} bv${size}urem(bv${size},bv${size}) returns(bv${size});
      |function {:bvbuiltin "bvsdiv"} bv${size}sdiv(bv${size},bv${size}) returns(bv${size});
      |function {:bvbuiltin "bvsrem"} bv${size}srem(bv${size},bv${size}) returns(bv${size});
      |function {:bvbuiltin "bvsmod"} bv${size}smod(bv${size},bv${size}) returns(bv${size});
      |function {:bvbuiltin "bvneg"} bv${size}neg(bv${size}) returns(bv${size});
      |
      |// Bitwise operations
      |function {:bvbuiltin "bvand"} bv${size}and(bv${size},bv${size}) returns(bv${size});
      |function {:bvbuiltin "bvor"} bv${size}or(bv${size},bv${size}) returns(bv${size});
      |function {:bvbuiltin "bvnot"} bv${size}not(bv${size}) returns(bv${size});
      |function {:bvbuiltin "bvxor"} bv${size}xor(bv${size},bv${size}) returns(bv${size});
      |function {:bvbuiltin "bvnand"} bv${size}nand(bv${size},bv${size}) returns(bv${size});
      |function {:bvbuiltin "bvnor"} bv${size}nor(bv${size},bv${size}) returns(bv${size});
      |function {:bvbuiltin "bvxnor"} bv${size}xnor(bv${size},bv${size}) returns(bv${size});
      |
      |// Bit shifting
      |function {:bvbuiltin "bvshl"} bv${size}shl(bv${size},bv${size}) returns(bv${size});
      |function {:bvbuiltin "bvlshr"} bv${size}lshr(bv${size},bv${size}) returns(bv${size});
      |function {:bvbuiltin "bvashr"} bv${size}ashr(bv${size},bv${size}) returns(bv${size});
      |
      |// Unsigned comparison
      |function {:bvbuiltin "bvult"} bv${size}ult(bv${size},bv${size}) returns(bool);
      |function {:bvbuiltin "bvule"} bv${size}ule(bv${size},bv${size}) returns(bool);
      |function {:bvbuiltin "bvugt"} bv${size}ugt(bv${size},bv${size}) returns(bool);
      |function {:bvbuiltin "bvuge"} bv${size}uge(bv${size},bv${size}) returns(bool);
      |
      |// Signed comparison
      |function {:bvbuiltin "bvslt"} bv${size}slt(bv${size},bv${size}) returns(bool);
      |function {:bvbuiltin "bvsle"} bv${size}sle(bv${size},bv${size}) returns(bool);
      |function {:bvbuiltin "bvsgt"} bv${size}sgt(bv${size},bv${size}) returns(bool);
      |function {:bvbuiltin "bvsge"} bv${size}sge(bv${size},bv${size}) returns(bool);
      |
      |function {:bvbuiltin "bvcomp"} bv${size}comp(bv${size},bv${size}) returns(bool);
      |
      |""".stripMargin

  def generateBVToBoolHeader = """
      |function booltobv1(bool) returns (bv1);
      |axiom booltobv1(true) == 1bv1 && booltobv1(false) == 0bv1;
      |
      |function bv1tobool(bv1) returns (bool);
      |axiom bv1tobool(1bv1) == true && bv1tobool(0bv1) == false;
      |""".stripMargin
}
