package astnodes.parameters

import astnodes.exp.variable.Register

trait Parameter(name: Register, register: Register) {
  override def toString: String = {
    name.toString + ": bv" + name.size.get
  }
}