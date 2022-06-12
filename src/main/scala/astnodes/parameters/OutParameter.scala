package astnodes.parameters

import astnodes.exp.variable.Register

case class OutParameter(name: Register, register: Register) extends Parameter(name, register) {
  override def toString: String = {
    String.format("%s: bv%d, Gamma_%s: bool", name, name.size.get, name)
  }
}