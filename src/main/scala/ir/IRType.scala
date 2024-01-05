package ir

import boogie._

sealed trait IRType(val name: String) {
  override def toString: String = name
  def toBoogie: BType
}

case object BoolType extends IRType("bool") {
  override def toBoogie: BType = BoolBType
}

case object IntType extends IRType("int") {
  override def toBoogie: BType = IntBType
}

case class BitVecType(size: Int) extends IRType("bv" + size) {
  override def toBoogie: BType = BitVecBType(size)
}

case class MapType(param: IRType, result: IRType) extends IRType(s"[$param]$result") {
  override def toBoogie: BType = MapBType(param.toBoogie, result.toBoogie)
}

def coerceToBool(e: Expr): Expr = {
  e.getType match {
    case BitVecType(s) => BinaryExpr(BVNEQ, e, BitVecLiteral(0, s))
    case IntType => BinaryExpr(IntNEQ, e, IntLiteral(0))
    case BoolType => e
    case MapType(_, _) => ???
  }
}
