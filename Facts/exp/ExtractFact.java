package Facts.exp;

import Facts.DatalogUtility;
import Facts.Fact;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

public class ExtractFact extends ExpFact {

    public int firstInt;
    public int secondInt;
    public VarFact variable;

    public ExtractFact(int firstInt, int secondInt, VarFact variable) {
        this.firstInt = firstInt;
        this.secondInt = secondInt;
        this.variable = variable;
    }

    public String toString() {
        return String.format("%s[%d:%d]", variable, firstInt, secondInt);
    }

    public List<Fact> toFactList() {
        List<Fact> factList = new ArrayList<>();
        factList.addAll(variable.toFactList());
        factList.add(this);
        return factList;
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
