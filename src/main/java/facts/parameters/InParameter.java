package facts.parameters;

import facts.exp.MemFact;
import facts.exp.VarFact;

public class InParameter extends Parameter {

    private MemFact alias;

    public InParameter(VarFact name, VarFact register) {
        super(name, register);
    }

    public MemFact getAlias() {
        return alias;
    }

    public void setAlias(MemFact alias) {
        this.alias = alias;
    }
}
