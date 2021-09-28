package Facts.misc;

import Facts.Fact;
import Facts.inst.InstFact;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class SuccessorFact extends Fact {

    public InstFact i1;
    public InstFact i2;

    public SuccessorFact(InstFact i1, InstFact i2) {
        this.i1 = i1;
        this.i2 = i2;
    }

    /*
    To omit redundancy, we treat successor facts a bit differently and avoid recursively adding the instructions it
    refers to, because it is guaranteed these instructions will appear earlier in any InstFact list.
     */
    public List<Fact> toFactList() {
        List<Fact> factList = new ArrayList<>();
        factList.add(this);
        return factList;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        SuccessorFact that = (SuccessorFact) o;
        return Objects.equals(i1, that.i1) && Objects.equals(i2, that.i2);
    }

    @Override
    public int hashCode() {
        return Objects.hash(i1, i2);
    }
}
