package Facts.Exp;

import Facts.Fact;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class ExtractFact extends ExpFact {

    private int firstInt;
    private int secondInt;
    private VarFact variable;

    public ExtractFact(int firstInt, int secondInt, VarFact variable) {
        this.firstInt = firstInt;
        this.secondInt = secondInt;
        this.variable = variable;
    }

    public int getFirstInt() {
        return firstInt;
    }

    public void setFirstInt(int firstInt) {
        this.firstInt = firstInt;
    }

    public int getSecondInt() {
        return secondInt;
    }

    public void setSecondInt(int secondInt) {
        this.secondInt = secondInt;
    }

    public VarFact getVariable() {
        return variable;
    }

    public void setVariable(VarFact variable) {
        this.variable = variable;
    }

    public List<Fact> toFactList() {
        List<Fact> factList = new ArrayList<>(variable.toFactList());
        factList.add(this);
        return factList;
    }

    @Override
    public String toString() {
        return String.format("%s[%d:%d]", variable, firstInt, secondInt);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        ExtractFact that = (ExtractFact) o;
        return firstInt == that.firstInt && secondInt == that.secondInt && Objects.equals(variable, that.variable);
    }

    @Override
    public int hashCode() {
        return Objects.hash(firstInt, secondInt, variable);
    }
}
