package astnodes;

case class Label(pc: String, visible: Boolean = false) {
  override def toString = if (visible)  String.format("label%s: ", pc) else ""
}
