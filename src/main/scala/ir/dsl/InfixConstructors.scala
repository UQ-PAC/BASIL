package ir.dsl
import ir.*
import ir.dsl.*

import scala.annotation.targetName
import scala.collection.immutable.*

/**
 * Expr and statement construction; this defined infix operators which construct
 * BASIL IR statements and expressions.
 *
 * Typically with binops we default to signed ops, and provide named unsigned alternatives.
 */

extension (lvar: Variable)
  infix def :=(j: Expr) = LocalAssign(lvar, j)
  def :=(j: Int) = lvar.getType match {
    case BitVecType(sz) => LocalAssign(lvar, BitVecLiteral(j, sz))
    case IntType => LocalAssign(lvar, IntLiteral(j))
    case _ => ???
  }
  def :=(j: Boolean) = lvar.getType match {
    case BoolType => LocalAssign(lvar, if j then TrueLiteral else FalseLiteral)
    case _ => ???
  }

case class call(target: String, actualParams: (String, Expr)*)

extension (lvar: List[(String, Variable)]) infix def :=(j: call) = directCall(lvar, j.target, j.actualParams: _*)
extension (lvar: Seq[(String, Variable)]) infix def :=(j: call) = directCall(lvar, j.target, j.actualParams: _*)

extension (v: Int)
  @targetName("ibv64")
  def bv64 = BitVecLiteral(v, 64)
  @targetName("ibv32")
  def bv32 = BitVecLiteral(v, 32)
  @targetName("ibv16")
  def bv16 = BitVecLiteral(v, 16)
  @targetName("ibv8")
  def bv8 = BitVecLiteral(v, 8)
  @targetName("ibv1")
  def bv1 = BitVecLiteral(v, 1)
  @targetName("itobv")
  def bv(sz: Int) = BitVecLiteral(v, sz)

def bv64 = BitVecType(64)
def bv32 = BitVecType(32)
def bv16 = BitVecType(16)
def bv8 = BitVecType(8)
def bv1 = BitVecType(1)

private def panic(op: String, i: Expr, j: Expr) = throw Exception(s"$op not defiend for (${i.getType}, ${j.getType})")

extension (i: Expr)

  infix def ===(j: Expr): Expr = BinaryExpr(EQ, i, j)
  infix def !==(j: Expr): Expr = BinaryExpr(NEQ, i, j)
  infix def +(j: Expr): Expr = i.getType match {
    case IntType => BinaryExpr(IntADD, i, j)
    case b: BitVecType => BinaryExpr(BVADD, i, j)
    case _ => panic("Addition", i, j)
  }
  infix def -(j: Expr): Expr = i.getType match {
    case IntType => BinaryExpr(IntSUB, i, j)
    case b: BitVecType => BinaryExpr(BVSUB, i, j)
    case _ => panic("Subtraction", i, j)
  }
  infix def *(j: Expr): Expr = i.getType match {
    case IntType => BinaryExpr(IntMUL, i, j)
    case b: BitVecType => BinaryExpr(BVMUL, i, j)
    case BoolType => BinaryExpr(BoolAND, i, j)
    case _ => panic("Multiplication", i, j)
  }
  infix def /(j: Expr): Expr = i.getType match {
    case IntType => BinaryExpr(IntDIV, i, j)
    case b: BitVecType => BinaryExpr(BVSDIV, i, j)
    case _ => panic("Signed Division", i, j)
  }
  infix def &&(j: Expr): Expr = i.getType match {
    case BoolType => BinaryExpr(BoolAND, i, j)
    case _ => panic("Logical AND", i, j)
  }
  infix def ||(j: Expr): Expr = i.getType match {
    case BoolType => BinaryExpr(BoolOR, i, j)
    case _ => panic("Logical OR", i, j)
  }
  infix def &(j: Expr): Expr = i.getType match {
    case b: BitVecType => BinaryExpr(BVAND, i, j)
    case _ => panic("Bitwise AND", i, j)
  }
  infix def |(j: Expr): Expr = i.getType match {
    case b: BitVecType => BinaryExpr(BVOR, i, j)
    case _ => panic("Bitwise OR", i, j)
  }
  infix def <<(j: Expr): Expr = i.getType match {
    case b: BitVecType => BinaryExpr(BVSHL, i, j)
    case _ => panic("Shift Left", i, j)
  }
  infix def >>(j: Expr): Expr = i.getType match {
    case b: BitVecType => BinaryExpr(BVASHR, i, j)
    case _ => panic("Arithmetic Shift Right", i, j)
  }
  infix def >>>(j: Expr): Expr = i.getType match {
    case b: BitVecType => BinaryExpr(BVLSHR, i, j)
    case _ => panic("Logical Shift Right", i, j)
  }
  infix def %(j: Expr): Expr = i.getType match {
    case IntType => BinaryExpr(IntMOD, i, j)
    case b: BitVecType => BinaryExpr(BVSMOD, i, j)
    case _ => panic("Signed Modulo", i, j)
  }
  infix def <(j: Expr): Expr = i.getType match {
    case IntType => BinaryExpr(IntLT, i, j)
    case b: BitVecType => BinaryExpr(BVSLT, i, j)
    case _ => panic("Less Than", i, j)
  }
  infix def >(j: Expr): Expr = i.getType match {
    case IntType => BinaryExpr(IntGT, i, j)
    case b: BitVecType => BinaryExpr(BVSGT, i, j)
    case _ => panic("Greater Than", i, j)
  }
  infix def <=(j: Expr): Expr = i.getType match {
    case IntType => BinaryExpr(IntLE, i, j)
    case b: BitVecType => BinaryExpr(BVSLE, i, j)
    case _ => panic("Less Than or Equal", i, j)
  }
  infix def >=(j: Expr): Expr = i.getType match {
    case IntType => BinaryExpr(IntGE, i, j)
    case b: BitVecType => BinaryExpr(BVSGE, i, j)
    case _ => panic("Greater Than or Equal", i, j)
  }
  infix def ult(j: Expr): Expr = i.getType match {
    case b: BitVecType => BinaryExpr(BVULT, i, j)
    case _ => panic("ULT", i, j)
  }
  infix def ugt(j: Expr): Expr = i.getType match {
    case b: BitVecType => BinaryExpr(BVUGT, i, j)
    case _ => panic("UGT", i, j)
  }
  infix def ule(j: Expr): Expr = i.getType match {
    case b: BitVecType => BinaryExpr(BVULE, i, j)
    case _ => panic("ULE", i, j)
  }
  infix def uge(j: Expr): Expr = i.getType match {
    case b: BitVecType => BinaryExpr(BVUGE, i, j)
    case _ => panic("UGE", i, j)
  }
  infix def ++(j: Expr): Expr = i.getType match {
    case b: BitVecType => BinaryExpr(BVCONCAT, i, j)
    case _ => panic("Concat Bits", i, j)
  }
