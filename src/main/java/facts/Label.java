package facts;

public class Label {

    private String pc;
    private boolean visible = false;

    public Label(String pc) {
        this.pc = pc;
    }

    public String getPc() {
        return pc;
    }

    public void setPc(String pc) {
        this.pc = pc;
    }

    public boolean isVisible() {
        return visible;
    }

    public void show() {
        visible = true;
    }

    public void hide() {
        visible = false;
    }

    @Override
    public String toString() {
        return visible ? String.format("label%s: ", pc) : "";
    }
}
