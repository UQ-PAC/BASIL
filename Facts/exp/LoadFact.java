package Facts.exp;

import Facts.Fact;

public class LoadFact extends ExpFact {
    /** Variable to load from (e.g. mem) */
    String var;
    /** Address to load from */
    String address;

    public LoadFact(String var, String address) {
        super();
        this.var = var;
        this.address = address;
    }

    public String toString() {
        return String.format("exp(%s,load,%s,%s,none)", super.id, var, address);
    }

}
