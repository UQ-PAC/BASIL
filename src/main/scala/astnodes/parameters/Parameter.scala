package astnodes.parameters

import astnodes.exp.`var`.Register

abstract class Parameter(newName: Register, register: Register) {
  private var name: Register = newName

  def getName: Register = {
    name
  }
    
  def setName(name: Register): Unit = {
    this.name = name
  }

  def getRegister: Register = {
    register
  }

  override def toString: String = {
    name.toString + ": bv" + name.size.get
  }
}