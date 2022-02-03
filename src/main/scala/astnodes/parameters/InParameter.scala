package astnodes.parameters

import astnodes.exp.`var`.Register;
import astnodes.exp.`var`.MemLoad;

class InParameter(var name: Register, register: Register) extends Parameter(name, register) {
    var alias: MemLoad

    def getAlias: MemLoad = {
        alias
    }

    def setAlias(alias: MemLoad) = {
        this.alias = alias
    }
}