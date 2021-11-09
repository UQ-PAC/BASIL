package Facts;

import Facts.exp.MemFact;
import Facts.exp.VarFact;
import com.sun.istack.internal.Nullable;

public class InParameter extends Parameter {

    private MemFact alias;

    public InParameter(VarFact name, VarFact register) {
        super(name, register);
    }

    @Nullable
    public MemFact getAlias() {
        return alias;
    }

    public void setAlias(MemFact alias) {
        this.alias = alias;
    }
}
