package Facts;

import java.util.Objects;

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

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Label label = (Label) o;
        return hide == label.hide && Objects.equals(pc, label.pc);
    }

    @Override
    public int hashCode() {
        return Objects.hash(pc, hide);
    }
}
