package Facts.exp;

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
}
