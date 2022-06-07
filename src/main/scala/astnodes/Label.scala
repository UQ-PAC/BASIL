package astnodes

case class Label(pc: String, visible: Boolean = false) {
  override def toString: String = if (visible)  String.format("label%s: ", pc) else ""
}
