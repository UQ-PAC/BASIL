package astnodes.parameters

import astnodes.exp.variable.{MemLoad, Register}

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