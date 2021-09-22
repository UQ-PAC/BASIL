package Facts.inst;

import Facts.Fact;
import Facts.exp.MemFact;
import Facts.exp.VarFact;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

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

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        ParamFact paramFact = (ParamFact) o;
        return is_result == paramFact.is_result && Objects.equals(name, paramFact.name) && Objects.equals(register, paramFact.register) && Objects.equals(alias, paramFact.alias);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), name, register, alias, is_result);
    }
}
