package facts.exp;

import facts.Fact;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 * Unary operator fact
 */
public class UopFact extends ExpFact {

    private String operator;
    private ExpFact exp;

    public UopFact(String operator, ExpFact exp) {
        this.operator = operator;
        this.exp = exp;
    }

    public String getOperator() {
        return operator;
    }

    public void setOperator(String operator) {
        this.operator = operator;
    }

    public ExpFact getExp() {
        return exp;
    }

    public void setExp(ExpFact exp) {
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
        UopFact uopFact = (UopFact) o;
        return Objects.equals(operator, uopFact.operator) && Objects.equals(exp, uopFact.exp);
    }

    @Override
    public int hashCode() {
        return Objects.hash(operator, exp);
    }

    @Override
    public List<ExpFact> getChildren() {
        return Collections.singletonList(exp);
    }

    @Override
    public void replace(ExpFact oldExp, ExpFact newExp) {
        if (exp.equals(oldExp)) {
            exp = newExp;
        }
    }
}
