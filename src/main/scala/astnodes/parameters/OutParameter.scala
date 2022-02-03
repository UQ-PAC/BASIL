package astnodes.parameters

import astnodes.exp.`var`.Register

class OutParameter(var name: Register, register: Register) extends Parameter(name, register) {
    override def toString: String = {
        String.format("%s: bv%d, Gamma_%s: bool", getName, getName.size.get, getName);
    }
}