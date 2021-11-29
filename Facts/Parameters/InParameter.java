package Facts.Parameters;

import Facts.Exp.MemFact;
import Facts.Exp.VarFact;

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
