package ir.dsl
import ir.*
import translating.PrettyPrinter.*
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.*
import scala.annotation.targetName
import ir.dsl.*

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

extension (i: Expr)
  infix def ===(j: Expr): Expr = i.getType match {
    case IntType => BinaryExpr(IntEQ, i, j)
    case b: BitVecType => BinaryExpr(BVEQ, i, j)
    case BoolType => BinaryExpr(BoolEQ, i, j)
    case m: MapType => ???
  }
  infix def !==(j: Expr): Expr = i.getType match {
    case IntType => BinaryExpr(IntNEQ, i, j)
    case b: BitVecType => BinaryExpr(BVNEQ, i, j)
    case BoolType => BinaryExpr(BoolNEQ, i, j)
    case m: MapType => ???
  }
  infix def +(j: Expr): Expr = i.getType match {
    case IntType => BinaryExpr(IntADD, i, j)
    case b: BitVecType => BinaryExpr(BVADD, i, j)
    case BoolType => BinaryExpr(BoolOR, i, j)
    case m: MapType => ???
  }
  infix def -(j: Expr): Expr = i.getType match {
    case IntType => BinaryExpr(IntSUB, i, j)
    case b: BitVecType => BinaryExpr(BVSUB, i, j)
    case BoolType => ???
    case m: MapType => ???
  }
  infix def *(j: Expr): Expr = i.getType match {
    case IntType => BinaryExpr(IntMUL, i, j)
    case b: BitVecType => BinaryExpr(BVMUL, i, j)
    case BoolType => BinaryExpr(BoolAND, i, j)
    case m: MapType => ???
  }
  infix def /(j: Expr): Expr = i.getType match {
    case IntType => BinaryExpr(IntDIV, i, j)
    case b: BitVecType => BinaryExpr(BVSDIV, i, j)
    case BoolType => ???
    case m: MapType => ???
  }
  infix def &&(j: Expr): Expr = i.getType match {
    case IntType => ???
    case b: BitVecType => BinaryExpr(BVAND, i, j)
    case BoolType => BinaryExpr(BoolAND, i, j)
    case m: MapType => ???
  }
  infix def ||(j: Expr): Expr = i.getType match {
    case IntType => ???
    case b: BitVecType => BinaryExpr(BVOR, i, j)
    case BoolType => BinaryExpr(BoolOR, i, j)
    case m: MapType => ???
  }
  infix def <<(j: Expr): Expr = i.getType match {
    case IntType => ???
    case b: BitVecType => BinaryExpr(BVSHL, i, j)
    case BoolType => ???
    case m: MapType => ???
  }
  infix def >>(j: Expr): Expr = i.getType match {
    case IntType => ???
    case b: BitVecType => BinaryExpr(BVASHR, i, j)
    case BoolType => ???
    case m: MapType => ???
  }
  infix def >>>(j: Expr): Expr = i.getType match {
    case IntType => ???
    case b: BitVecType => BinaryExpr(BVLSHR, i, j)
    case BoolType => ???
    case m: MapType => ???
  }
  infix def %(j: Expr): Expr = i.getType match {
    case IntType => BinaryExpr(IntMOD, i, j)
    case b: BitVecType => BinaryExpr(BVSMOD, i, j)
    case BoolType => ???
    case m: MapType => ???
  }
  infix def <(j: Expr): Expr = i.getType match {
    case IntType => BinaryExpr(IntLT, i, j)
    case b: BitVecType => BinaryExpr(BVSLT, i, j)
    case BoolType => ???
    case m: MapType => ???
  }
  infix def >(j: Expr): Expr = i.getType match {
    case IntType => BinaryExpr(IntGT, i, j)
    case b: BitVecType => BinaryExpr(BVSGT, i, j)
    case BoolType => ???
    case m: MapType => ???
  }
  infix def <=(j: Expr): Expr = i.getType match {
    case IntType => BinaryExpr(IntLE, i, j)
    case b: BitVecType => BinaryExpr(BVSLE, i, j)
    case BoolType => ???
    case m: MapType => ???
  }
  infix def >=(j: Expr): Expr = i.getType match {
    case IntType => BinaryExpr(IntGE, i, j)
    case b: BitVecType => BinaryExpr(BVSGE, i, j)
    case BoolType => ???
    case m: MapType => ???
  }
  infix def ult(j: Expr): Expr = i.getType match {
    case IntType => ???
    case b: BitVecType => BinaryExpr(BVULT, i, j)
    case BoolType => ???
    case m: MapType => ???
  }
  infix def ugt(j: Expr): Expr = i.getType match {
    case IntType => ???
    case b: BitVecType => BinaryExpr(BVUGT, i, j)
    case BoolType => ???
    case m: MapType => ???
  }
  infix def ule(j: Expr): Expr = i.getType match {
    case IntType => ???
    case b: BitVecType => BinaryExpr(BVULE, i, j)
    case BoolType => ???
    case m: MapType => ???
  }
  infix def uge(j: Expr): Expr = i.getType match {
    case IntType => ???
    case b: BitVecType => BinaryExpr(BVUGE, i, j)
    case BoolType => ???
    case m: MapType => ???
  }
  infix def ++(j: Expr): Expr = i.getType match {
    case IntType => ???
    case b: BitVecType => BinaryExpr(BVCONCAT, i, j)
    case BoolType => ???
    case m: MapType => ???
  }
