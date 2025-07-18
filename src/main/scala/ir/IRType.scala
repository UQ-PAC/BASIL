package ir

import boogie.*

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

case class CustomSort(sortName: String) extends IRType(sortName) {
  override def toBoogie: BType = CustomBType(name)
}

case class BitVecType(size: Int) extends IRType("bv" + size) {
  override def toBoogie: BType = BitVecBType(size)
  def maxValue = BigInt(2).pow(size) - 1
}

case class MapType(param: IRType, result: IRType) extends IRType(s"[$param]$result") {
  override def toBoogie: BType = MapBType(param.toBoogie, result.toBoogie)
  override def toString() = s"($param -> $result)"
}

def uncurryFunctionType(params: Iterable[IRType], body: IRType): IRType = {
  // maybe a bit odd given we have no no unit type
  params.toList.reverse.foldLeft(body)((acc, param) => MapType(param, acc))
}

def curryFunctionType(t: IRType, acc: List[IRType] = List()): (List[IRType], IRType) = t match {
  case MapType(arg, t) => curryFunctionType(t, arg :: acc)
  case t => (acc.reverse, t)
}

def coerceToBool(e: Expr): Expr = {
  e.getType match {
    case BitVecType(s) => BinaryExpr(NEQ, e, BitVecLiteral(0, s))
    case IntType => BinaryExpr(NEQ, e, IntLiteral(0))
    case BoolType => e
    case MapType(_, _) => ???
    case _ => ???
  }
}
