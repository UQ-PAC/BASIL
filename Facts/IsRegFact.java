package Facts;

public class IsRegFact extends Fact{
    String id;

    public IsRegFact(String id) {
        this.id = id;
    }

    public String toString() {
        return String.format("isReg(%s)", id);
    }
}
