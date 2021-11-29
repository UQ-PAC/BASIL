package facts.exp;

import facts.Fact;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * Literal expression (e.g. 4, 5, 10)
 */
public class LiteralFact extends ExpFact {
    /** Value of literal */
    private String val;

    public LiteralFact(String val) {
        this.val = parseHex(val);
    }

    private String parseHex (String val) {
        if (val.length() < 3 || !val.substring(0, 2).equals("0x")) return val;

        return Long.toString(Long.parseUnsignedLong(val.substring(2), 16));
    }

    public String getVal() {
        return val;
    }

    public void setVal(String val) {
        this.val = val;
    }

    public List<Fact> toFactList() {
        List<Fact> factList = new ArrayList<>();
        factList.add(this);
        return factList;
    }

    @Override
    public String toString() {
        return String.format("%s", val);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        LiteralFact that = (LiteralFact) o;
        return Objects.equals(val, that.val);
    }

    @Override
    public int hashCode() {
        return Objects.hash(val);
    }

    @Override
    public List<ExpFact> getChildren() {
        return new ArrayList<>();
    }

    @Override
    public void replace(ExpFact oldExp, ExpFact newExp) {}
}
