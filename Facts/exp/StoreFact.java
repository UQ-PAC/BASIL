package Facts.exp;

import Facts.Fact;

public class StoreFact extends ExpFact {
    /** Variable to store into (e.g. mem) */
    String var;
    /** Address to store at */
    String address;
    /** Value to store */
    String value;

    public StoreFact(String var, String address, String value) {
        super();
        this.var = var;
        this.address = address;
        this.value = value;
    }

    public String toString() {
        return String.format("exp(%s,store,%s,%s,%s", super.id, var, address, value);
    }

}
