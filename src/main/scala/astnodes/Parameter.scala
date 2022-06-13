package astnodes

trait Parameter(name: Register, register: Register) {
  override def toString: String = {
    name.toString + ": bv" + name.size.get
  }
}

case class OutParameter(name: Register, register: Register) extends Parameter(name, register) {
  override def toString: String = {
    String.format("%s: bv%d, Gamma_%s: bool", name, name.size.get, name)
  }
}

case class InParameter(name: Register, register: Register) extends Parameter(name, register) {
  /*  
  var alias: MemLoad = null

  def getAlias: MemLoad = {
    alias
  }

  def setAlias(alias: MemLoad): Unit = {
    this.alias = alias
  }
  */
}