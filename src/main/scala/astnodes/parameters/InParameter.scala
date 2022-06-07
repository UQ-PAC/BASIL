package astnodes.parameters

import astnodes.exp.`var`.Register
import astnodes.exp.`var`.MemLoad

class InParameter(inName: Register, register: Register) extends Parameter(inName, register) {
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