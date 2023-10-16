package boogie

sealed trait BType(val name: String) {
  override def toString: String = name
}

case object BoolBType extends BType("bool")

case object IntBType extends BType("int")

case class BitVecBType(size: Int) extends BType("bv" + size)

case class MapBType(param: BType, result: BType) extends BType(s"[$param]$result")

case object SpecType extends BType("spec")
