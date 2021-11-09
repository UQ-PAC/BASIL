package Facts;

import Facts.exp.MemFact;
import Facts.exp.VarFact;
import com.sun.istack.internal.Nullable;

public class Parameter {

    private VarFact name;
    private final VarFact register;
    private MemFact alias;
    private final boolean isResult;

    public Parameter(VarFact name, VarFact register, boolean isResult) {
        this.name = name;
        this.register = register;
        this.isResult = isResult;
    }

    public VarFact getName() {
        return name;
    }

    public void setName(VarFact name) {
        this.name = name;
    }

    public VarFact getRegister() {
        return register;
    }

    @Nullable
    public MemFact getAlias() {
        return alias;
    }

    public void setAlias(MemFact alias) {
        this.alias = alias;
    }

    public boolean isResult() {
        return isResult;
    }

    @Override
    public String toString() {
        return name.toString();
    }
}
