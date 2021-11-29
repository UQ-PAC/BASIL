package facts.stmt;

import java.util.ArrayList;
import facts.exp.Expr;
import java.util.List;

public class ExitSub extends Stmt {

    public ExitSub(String pc) {
        super(pc);
    }

    @Override
    public String toString() {
        return "return;";
    }

    @Override
    public List<Expr> getChildren() {
        return new ArrayList<>();
    }

    @Override
    public void replace(Expr oldExp, Expr newExp) {}
}
