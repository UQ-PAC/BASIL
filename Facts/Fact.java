package Facts;
public abstract class Fact {

    public int id;

    @Override
    public int hashCode() {
        return this.toString().hashCode();
    }

    @Override
    public boolean equals(Object object) {
        if (object == null || object.getClass() != this.getClass()) {
            return false;
        }
        return object.toString().equals(this.toString());

    }
}
