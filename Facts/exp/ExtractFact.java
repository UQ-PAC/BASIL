package Facts.exp;

import java.util.ArrayList;
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

    public List<String> toDatalog() {
        List<String> log = new ArrayList<>(variable.toDatalog());
        log.add(String.format("$%s\t%s\t%s\t%s\t%s", super.id, "extract", firstInt, secondInt, variable.id));
        return log;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        ExtractFact that = (ExtractFact) o;
        return firstInt == that.firstInt && secondInt == that.secondInt && Objects.equals(variable, that.variable);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), firstInt, secondInt, variable);
    }
}
