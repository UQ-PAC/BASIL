package Facts;

public class Label {

    public String pc;
    public boolean hide = true;

    public Label(String pc) {
        this.pc = pc;
    }

    @Override
    public String toString() {
        return hide ? "" : String.format("label%s: ", pc);
    }
}
