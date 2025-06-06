package ir.parsing

import basil_ir.{Absyn => syntax}

import scala.jdk.CollectionConverters.*

trait LiteralsBNFCVisitor[A]
    extends syntax.BinOp.Visitor[BasilParseValue, A],
      syntax.BVLogicalBinOp.Visitor[BasilParseValue, A],
      syntax.IntLogicalBinOp.Visitor[BasilParseValue, A],
      syntax.BVBinOp.Visitor[BasilParseValue, A],
      syntax.IntBinOp.Visitor[BasilParseValue, A],
      syntax.BoolBinOp.Visitor[BasilParseValue, A],
      syntax.BVUnOp.Visitor[BasilParseValue, A],
      syntax.UnOp.Visitor[BasilParseValue, A],
      syntax.EqOp.Visitor[BasilParseValue, A],
      syntax.Endian.Visitor[BasilParseValue, A],
      syntax.IntVal.Visitor[BasilParseValue, A],
      syntax.PAddress.Visitor[BasilParseValue, A],
      syntax.PEntry.Visitor[BasilParseValue, A] {

  def unquote(s: String, x: HasParsePosition) = s match {
    case s"\"$x\"" => x
    case _ => throw ParseException("invalid quoted string", x)
  }

  import scala.language.implicitConversions

  // Members declared in PAddress.Visitor
  override def visit(x: syntax.AddrSome, arg: A): BasilParseValue =
    Some(x.intval_.accept(this, arg).int)
  override def visit(x: syntax.AddrNone, arg: A): BasilParseValue = None

  // Members declared in PEntry.Visitor
  override def visit(x: syntax.EntrySome, arg: A): BasilParseValue = Some(x.str_)
  override def visit(x: syntax.EntryNone, arg: A): BasilParseValue = None

  // Members declared in BinOp.Visitor
  override def visit(x: syntax.BinOpBVBinOp, arg: A): BasilParseValue = x.bvbinop_.accept(this, arg)
  override def visit(x: syntax.BinOpBVLogicalBinOp, arg: A): BasilParseValue = x.bvlogicalbinop_.accept(this, arg)
  override def visit(x: syntax.BinOpBoolBinOp, arg: A): BasilParseValue = x.boolbinop_.accept(this, arg)
  override def visit(x: syntax.BinOpIntLogicalBinOp, arg: A): BasilParseValue = x.intlogicalbinop_.accept(this, arg)
  override def visit(x: syntax.BinOpIntBinOp, arg: A): BasilParseValue = x.intbinop_.accept(this, arg)
  override def visit(x: syntax.BinOpEqOp, arg: A): BasilParseValue = x.eqop_.accept(this, arg)

  // Members declared in EqOp.Visitor
  override def visit(x: syntax.EqOp_eq, arg: A): BasilParseValue = ir.EQ
  override def visit(x: syntax.EqOp_neq, arg: A): BasilParseValue = ir.NEQ

  // Members declared in BVLogicalBinOp.Visitor
  override def visit(x: syntax.BVLogicalBinOp_bvult, arg: A): BasilParseValue = ir.BVULT
  override def visit(x: syntax.BVLogicalBinOp_bvule, arg: A): BasilParseValue = ir.BVULE
  override def visit(x: syntax.BVLogicalBinOp_bvugt, arg: A): BasilParseValue = ir.BVUGT
  override def visit(x: syntax.BVLogicalBinOp_bvuge, arg: A): BasilParseValue = ir.BVUGE
  override def visit(x: syntax.BVLogicalBinOp_bvslt, arg: A): BasilParseValue = ir.BVSLT
  override def visit(x: syntax.BVLogicalBinOp_bvsle, arg: A): BasilParseValue = ir.BVSLE
  override def visit(x: syntax.BVLogicalBinOp_bvsgt, arg: A): BasilParseValue = ir.BVSGT
  override def visit(x: syntax.BVLogicalBinOp_bvsge, arg: A): BasilParseValue = ir.BVSGE

  // Members declared in IntLogicalBinOp.Visitor
  override def visit(x: syntax.IntLogicalBinOp_intlt, arg: A): BasilParseValue = ir.IntLT
  override def visit(x: syntax.IntLogicalBinOp_intle, arg: A): BasilParseValue = ir.IntLE
  override def visit(x: syntax.IntLogicalBinOp_intgt, arg: A): BasilParseValue = ir.IntGT
  override def visit(x: syntax.IntLogicalBinOp_intge, arg: A): BasilParseValue = ir.IntGE

  // Members declared in BVBinOp.Visitor
  override def visit(x: syntax.BVBinOp_bvand, arg: A): BasilParseValue = ir.BVAND
  override def visit(x: syntax.BVBinOp_bvor, arg: A): BasilParseValue = ir.BVOR
  override def visit(x: syntax.BVBinOp_bvadd, arg: A): BasilParseValue = ir.BVADD
  override def visit(x: syntax.BVBinOp_bvmul, arg: A): BasilParseValue = ir.BVMUL
  override def visit(x: syntax.BVBinOp_bvudiv, arg: A): BasilParseValue = ir.BVUDIV
  override def visit(x: syntax.BVBinOp_bvurem, arg: A): BasilParseValue = ir.BVUREM
  override def visit(x: syntax.BVBinOp_bvshl, arg: A): BasilParseValue = ir.BVSHL
  override def visit(x: syntax.BVBinOp_bvlshr, arg: A): BasilParseValue = ir.BVLSHR
  override def visit(x: syntax.BVBinOp_bvnand, arg: A): BasilParseValue = ir.BVNAND
  override def visit(x: syntax.BVBinOp_bvnor, arg: A): BasilParseValue = ir.BVNOR
  override def visit(x: syntax.BVBinOp_bvxor, arg: A): BasilParseValue = ir.BVXOR
  override def visit(x: syntax.BVBinOp_bvxnor, arg: A): BasilParseValue = ir.BVXNOR
  override def visit(x: syntax.BVBinOp_bvcomp, arg: A): BasilParseValue = ir.BVCOMP
  override def visit(x: syntax.BVBinOp_bvsub, arg: A): BasilParseValue = ir.BVSUB
  override def visit(x: syntax.BVBinOp_bvsdiv, arg: A): BasilParseValue = ir.BVSDIV
  override def visit(x: syntax.BVBinOp_bvsrem, arg: A): BasilParseValue = ir.BVSREM
  override def visit(x: syntax.BVBinOp_bvsmod, arg: A): BasilParseValue = ir.BVSMOD
  override def visit(x: syntax.BVBinOp_bvashr, arg: A): BasilParseValue = ir.BVASHR
  /*
   use enum value from name
   f_��5lywAir.pbv$UBxxxj0
   */

  // Members declared in IntBinOp.Visitor
  override def visit(x: syntax.IntBinOp_intadd, arg: A): BasilParseValue = ir.IntADD
  override def visit(x: syntax.IntBinOp_intmul, arg: A): BasilParseValue = ir.IntMUL
  override def visit(x: syntax.IntBinOp_intsub, arg: A): BasilParseValue = ir.IntSUB
  override def visit(x: syntax.IntBinOp_intdiv, arg: A): BasilParseValue = ir.IntDIV
  override def visit(x: syntax.IntBinOp_intmod, arg: A): BasilParseValue = ir.IntMOD

  // Members declared in BoolBinOp.Visitor
  override def visit(x: syntax.BoolBinOp_booland, arg: A): BasilParseValue = ir.BoolAND
  override def visit(x: syntax.BoolBinOp_boolor, arg: A): BasilParseValue = ir.BoolOR
  override def visit(x: syntax.BoolBinOp_boolimplies, arg: A): BasilParseValue = ir.BoolIMPLIES

  // Members declared in BVUnOp.Visitor
  override def visit(x: syntax.BVUnOp_bvnot, arg: A): BasilParseValue = ir.BVNOT
  override def visit(x: syntax.BVUnOp_bvneg, arg: A): BasilParseValue = ir.BVNEG

  // Members declared in UnOp.Visitor
  override def visit(x: syntax.UnOpBVUnOp, arg: A): BasilParseValue = x.bvunop_.accept(this, arg)
  override def visit(x: syntax.UnOp_boolnot, arg: A): BasilParseValue = ir.BoolNOT
  override def visit(x: syntax.UnOp_intneg, arg: A): BasilParseValue = ir.IntNEG
  override def visit(x: syntax.UnOp_booltobv1, arg: A): BasilParseValue = ir.BoolToBV1

  // Members declared in Endian.Visitor
  override def visit(x: syntax.LittleEndian, arg: A): BasilParseValue = ir.Endian.LittleEndian
  override def visit(x: syntax.BigEndian, arg: A): BasilParseValue = ir.Endian.BigEndian

  // Members declared in IntVal.Visitor
  override def visit(x: syntax.HexInt, arg: A): BasilParseValue =
    BigInt(x.integerhex_.toLowerCase.stripPrefix("0x"), 16)
  override def visit(x: syntax.DecInt, arg: A): BasilParseValue = BigInt(x.integer_) // XXX: int32

}

private def ensureTraitHasNoAbstractMembers[A] = new LiteralsBNFCVisitor[A] {}
