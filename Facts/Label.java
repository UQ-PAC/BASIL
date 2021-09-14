package Facts;

public class Label {

    String pc;
    boolean hide = true;

    public Label(String pc) {
        this.pc = pc;
    }

    @Override
    public String toString() {
        return hide ? "" : String.format("label%s: ", pc);
    }
}
