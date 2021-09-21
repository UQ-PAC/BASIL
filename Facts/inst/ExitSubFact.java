package Facts.inst;

import java.util.Objects;

public class ExitSubFact extends InstFact {

    public String funcName;

    public ExitSubFact(String pc, String funcName) {
        super(pc);
        this.funcName = funcName;
    }

    public String toString() {
        return "}\n\n";
    }

    public String toDatalog() {
        return String.format("%s\t%s\t%s\t%s", super.id, "exitfunc", funcName, "none");
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        ExitSubFact that = (ExitSubFact) o;
        return Objects.equals(funcName, that.funcName);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), funcName);
    }
}
