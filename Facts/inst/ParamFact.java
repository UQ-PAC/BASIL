package Facts.inst;

import Facts.Fact;
import Facts.exp.MemFact;
import Facts.exp.VarFact;
import java.util.ArrayList;
import java.util.List;

public class ParamFact extends InstFact {

    public VarFact name;
    public VarFact register;
    public MemFact alias;
    public boolean is_result;


    public ParamFact(String pc, VarFact name, VarFact register, boolean is_result) {
        super(pc);
        this.name = name;
        this.register = register;
        this.is_result = is_result;
    }

    public String toString() {
        return "";
    }

    public List<Fact> toFactList() {
        List<Fact> factList = new ArrayList<>();
        factList.add(this);
        return factList;
    }
}
