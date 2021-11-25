package facts.stmt;

import facts.exp.Expr;
import facts.Fact;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class ExitSub extends Stmt {

    public ExitSub(String pc) {
        super(pc);
    }

    public List<Fact> toFactList() {
        return Collections.singletonList(this);
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
