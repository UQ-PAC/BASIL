package ir.parsing

import basil_ir.Absyn as syntax
import ir.Sigil

trait LiteralsBNFCVisitor[A]
    extends syntax.BinOp.Visitor[ir.BinOp, A],
      syntax.BVLogicalBinOp.Visitor[ir.BVBinOp, A],
      syntax.IntLogicalBinOp.Visitor[ir.IntBinOp, A],
      syntax.BVBinOp.Visitor[ir.BVBinOp, A],
      syntax.IntBinOp.Visitor[ir.IntBinOp, A],
      syntax.BoolBinOp.Visitor[ir.BoolBinOp, A],
      syntax.EqOp.Visitor[ir.BinOp, A],
      syntax.BVUnOp.Visitor[ir.BVUnOp, A],
      syntax.UnOp.Visitor[ir.UnOp, A],
      syntax.Endian.Visitor[ir.Endian, A],
      syntax.IntVal.Visitor[BigInt, A],
      syntax.BVVal.Visitor[ir.BitVecLiteral, A],
      syntax.Value.Visitor[ir.Literal, A],
      TypesBNFCVisitor[A] {

  def unsigilBlock(x: String) = Sigil.unsigil(Sigil.BASIR.block)(x)
  def unsigilProc(x: String) = Sigil.unsigil(Sigil.BASIR.proc)(x)
  def unsigilLocal(x: String) =
    /* keep sigil as it is optional, to preserve var name */
    x
  def unsigilGlobal(x: String) = Sigil.unsigil(Sigil.BASIR.globalVar)(x)
  def unsigilAttrib(x: String) = Sigil.unsigil(Sigil.BASIR.attrib)(x)

  /**
   * Naive unquoting of double-quoted string. Does not do any
   * interpretation of escape sequences.
   */
  def unquote(s: String, x: HasParsePosition) = s match {
    case s"\"$s\"" => s
    case _ => throw ParseException("invalid quoted string", x)
  }

  // Members declared in BinOp.Visitor
  override def visit(x: syntax.BinOpBVBinOp, arg: A) = x.bvbinop_.accept(this, arg)
  override def visit(x: syntax.BinOpBVLogicalBinOp, arg: A) = x.bvlogicalbinop_.accept(this, arg)
  override def visit(x: syntax.BinOpBoolBinOp, arg: A) = x.boolbinop_.accept(this, arg)
  override def visit(x: syntax.BinOpIntLogicalBinOp, arg: A) = x.intlogicalbinop_.accept(this, arg)
  override def visit(x: syntax.BinOpIntBinOp, arg: A) = x.intbinop_.accept(this, arg)
  override def visit(x: syntax.BinOpEqOp, arg: A) = x.eqop_.accept(this, arg)

  // Members declared in EqOp.Visitor
  override def visit(x: syntax.EqOp_eq, arg: A) = ir.EQ
  override def visit(x: syntax.EqOp_neq, arg: A) = ir.NEQ

  // Members declared in BVLogicalBinOp.Visitor
  override def visit(x: syntax.BVLogicalBinOp_bvult, arg: A) = ir.BVULT
  override def visit(x: syntax.BVLogicalBinOp_bvule, arg: A) = ir.BVULE
  override def visit(x: syntax.BVLogicalBinOp_bvugt, arg: A) = ir.BVUGT
  override def visit(x: syntax.BVLogicalBinOp_bvuge, arg: A) = ir.BVUGE
  override def visit(x: syntax.BVLogicalBinOp_bvslt, arg: A) = ir.BVSLT
  override def visit(x: syntax.BVLogicalBinOp_bvsle, arg: A) = ir.BVSLE
  override def visit(x: syntax.BVLogicalBinOp_bvsgt, arg: A) = ir.BVSGT
  override def visit(x: syntax.BVLogicalBinOp_bvsge, arg: A) = ir.BVSGE

  // Members declared in IntLogicalBinOp.Visitor
  override def visit(x: syntax.IntLogicalBinOp_intlt, arg: A) = ir.IntLT
  override def visit(x: syntax.IntLogicalBinOp_intle, arg: A) = ir.IntLE
  override def visit(x: syntax.IntLogicalBinOp_intgt, arg: A) = ir.IntGT
  override def visit(x: syntax.IntLogicalBinOp_intge, arg: A) = ir.IntGE

  // Members declared in BVBinOp.Visitor
  override def visit(x: syntax.BVBinOp_bvand, arg: A) = ir.BVAND
  override def visit(x: syntax.BVBinOp_bvor, arg: A) = ir.BVOR
  override def visit(x: syntax.BVBinOp_bvadd, arg: A) = ir.BVADD
  override def visit(x: syntax.BVBinOp_bvmul, arg: A) = ir.BVMUL
  override def visit(x: syntax.BVBinOp_bvudiv, arg: A) = ir.BVUDIV
  override def visit(x: syntax.BVBinOp_bvurem, arg: A) = ir.BVUREM
  override def visit(x: syntax.BVBinOp_bvshl, arg: A) = ir.BVSHL
  override def visit(x: syntax.BVBinOp_bvlshr, arg: A) = ir.BVLSHR
  override def visit(x: syntax.BVBinOp_bvnand, arg: A) = ir.BVNAND
  override def visit(x: syntax.BVBinOp_bvnor, arg: A) = ir.BVNOR
  override def visit(x: syntax.BVBinOp_bvxor, arg: A) = ir.BVXOR
  override def visit(x: syntax.BVBinOp_bvxnor, arg: A) = ir.BVXNOR
  override def visit(x: syntax.BVBinOp_bvcomp, arg: A) = ir.BVCOMP
  override def visit(x: syntax.BVBinOp_bvsub, arg: A) = ir.BVSUB
  override def visit(x: syntax.BVBinOp_bvsdiv, arg: A) = ir.BVSDIV
  override def visit(x: syntax.BVBinOp_bvsrem, arg: A) = ir.BVSREM
  override def visit(x: syntax.BVBinOp_bvsmod, arg: A) = ir.BVSMOD
  override def visit(x: syntax.BVBinOp_bvashr, arg: A) = ir.BVASHR

  // Members declared in IntBinOp.Visitor
  override def visit(x: syntax.IntBinOp_intadd, arg: A) = ir.IntADD
  override def visit(x: syntax.IntBinOp_intmul, arg: A) = ir.IntMUL
  override def visit(x: syntax.IntBinOp_intsub, arg: A) = ir.IntSUB
  override def visit(x: syntax.IntBinOp_intdiv, arg: A) = ir.IntDIV
  override def visit(x: syntax.IntBinOp_intmod, arg: A) = ir.IntMOD

  // Members declared in BoolBinOp.Visitor
  override def visit(x: syntax.BoolBinOp_booland, arg: A) = ir.BoolAND
  override def visit(x: syntax.BoolBinOp_boolor, arg: A) = ir.BoolOR
  override def visit(x: syntax.BoolBinOp_boolimplies, arg: A) = ir.BoolIMPLIES

  // Members declared in BVUnOp.Visitor
  override def visit(x: syntax.BVUnOp_bvnot, arg: A) = ir.BVNOT
  override def visit(x: syntax.BVUnOp_bvneg, arg: A) = ir.BVNEG

  // Members declared in UnOp.Visitor
  override def visit(x: syntax.UnOpBVUnOp, arg: A) = x.bvunop_.accept(this, arg)
  override def visit(x: syntax.UnOp_boolnot, arg: A) = ir.BoolNOT
  override def visit(x: syntax.UnOp_intneg, arg: A) = ir.IntNEG
  override def visit(x: syntax.UnOp_booltobv1, arg: A) = ir.BoolToBV1

  // Members declared in Endian.Visitor
  override def visit(x: syntax.Endian_Little, arg: A) = ir.Endian.LittleEndian
  override def visit(x: syntax.Endian_Big, arg: A) = ir.Endian.BigEndian

  // Members declared in IntVal.Visitor
  override def visit(x: syntax.IntVal_Hex, arg: A) =
    BigInt(x.integerhex_.toLowerCase.stripPrefix("0x"), 16)
  override def visit(x: syntax.IntVal_Dec, arg: A) = BigInt(x.integerdec_)

  override def visit(x: syntax.BVVal1, arg: A): ir.BitVecLiteral =
    ir.BitVecLiteral(x.intval_.accept(this, arg), x.bvtype_.accept(this, arg).asInstanceOf[ir.BitVecType].size)

  override def visit(x: syntax.Value_BV, arg: A): ir.Literal = x.bvval_.accept(this, arg)
  override def visit(x: syntax.Value_Int, arg: A): ir.Literal = ir.IntLiteral(x.intval_.accept(this, arg))
  override def visit(x: syntax.Value_True, arg: A): ir.Literal = ir.TrueLiteral
  override def visit(x: syntax.Value_False, arg: A): ir.Literal = ir.FalseLiteral

}

private def ensureLiteralsVisitorHasNoAbstractMembers[A] = new LiteralsBNFCVisitor[A] {}
