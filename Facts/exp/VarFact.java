package Facts.exp;

public class VarFact extends ExpFact {
    String name;
    public VarFact(String name) {
        super();
        this.name = name;
    }

    public String toString() {
        return String.format("exp(%s,var,%s,none,none)", super.id, name);
    }
}
