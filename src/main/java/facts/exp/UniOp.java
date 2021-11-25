package facts.exp;

import facts.Fact;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 * Unary operator fact
 */
public class UniOp extends Expr {

    private String operator;
    private Expr exp;

    public UniOp(String operator, Expr exp) {
        this.operator = operator;
        this.exp = exp;
    }

    public String getOperator() {
        return operator;
    }

    public void setOperator(String operator) {
        this.operator = operator;
    }

    public Expr getExp() {
        return exp;
    }

    public void setExp(Expr exp) {
        this.exp = exp;
    }

    public List<Fact> toFactList() {
        List<Fact> factList = new ArrayList<>(exp.toFactList());
        factList.add(this);
        return factList;
    }

    @Override
    public String toString() {
        return String.format("%s %s", operator, exp);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        UniOp uniOp = (UniOp) o;
        return Objects.equals(operator, uniOp.operator) && Objects.equals(exp, uniOp.exp);
    }

    @Override
    public int hashCode() {
        return Objects.hash(operator, exp);
    }

    @Override
    public List<Expr> getChildren() {
        return Collections.singletonList(exp);
    }

    @Override
    public void replace(Expr oldExp, Expr newExp) {
        if (exp.equals(oldExp)) {
            exp = newExp;
        }
    }
}
