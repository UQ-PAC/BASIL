package Facts.inst;

/**
 * No instruction fact
 */
public class NopFact extends InstFact {

    public NopFact(String pc) {
        super(pc);
    }

    public String toString() {
        return (String.format("%sskip;\n", label));
    }

    public String toDatalog() {
        return String.format("%s\t%s\t%s\t%s", super.id, "skip", "none", "none");
    }

    @Override
    public boolean equals(Object o) {
        return super.equals(o);
    }
}
