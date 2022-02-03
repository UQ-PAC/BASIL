package astnodes.parameters

import astnodes.exp.`var`.Register;

abstract class Parameter(var name: Register, register: Register) {
    def getName: Register = {
        name
    }
    
    def setName(name: Register) = {
        this.name = name
    }

    def getRegister: Register = {
        register
    }

    override def toString: String = {
        name.toString + ": bv" + name.size.get
    }
}