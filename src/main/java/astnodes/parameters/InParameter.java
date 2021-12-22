package astnodes.parameters;

import astnodes.exp.var.MemLoad;
import astnodes.exp.var.Register;

public class InParameter extends Parameter {

    private MemLoad alias;

    public InParameter(Register name, Register register) {
        super(name, register);
    }

    public MemLoad getAlias() {
        return alias;
    }

    public void setAlias(MemLoad alias) {
        this.alias = alias;
    }
}
