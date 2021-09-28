package Facts.misc;

import Facts.Fact;

import java.util.Objects;

public class IsRegFact extends Fact {
    String id;

    public IsRegFact(String id) {
        this.id = id;
    }

    public String toString() {
        return String.format("isReg(%s)", id);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        IsRegFact isRegFact = (IsRegFact) o;
        return Objects.equals(id, isRegFact.id);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), id);
    }
}
