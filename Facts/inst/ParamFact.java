package Facts.inst;

import Facts.exp.VarFact;

public class ParamFact extends InstFact {

    public VarFact name;
    public VarFact register;
    public boolean is_result;


    public ParamFact(String pc, VarFact name, VarFact register, boolean is_result) {
        super(pc);
        this.name = name;
        this.register = register;
        this.is_result = is_result;
    }

    public String toString() {
        String toFrom = is_result ? "to" : "from";
        return String.format("%sparam %s, %s %s;", label, name, toFrom, register);
    }
}
