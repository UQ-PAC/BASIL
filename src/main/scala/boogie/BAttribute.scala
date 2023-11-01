package boogie

trait HasAttributes {
  def attributes: List[BAttribute]

  val attrString: String = if attributes.nonEmpty then {
    attributes.mkString(" ") + " "
  } else {
    ""
  }
}

case class BAttribute(name: String, value: Option[String] = None) {
  private val valueStr = if value.nonEmpty then s" ${value.get}" else ""
  override def toString: String = s"{:$name$valueStr}"
}