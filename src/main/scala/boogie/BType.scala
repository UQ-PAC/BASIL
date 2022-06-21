package boogie

sealed trait BType(val name: String) {
  override def toString: String = name
}

case object BoolType extends BType("bool")

case object IntType extends BType("int")

case class BitVec(size: Int) extends BType("bv" + size)

case class MapType(param: BType, result: BType) extends BType(s"[$param]$result")