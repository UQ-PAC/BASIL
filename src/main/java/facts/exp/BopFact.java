package Facts.Exp;

import Facts.Fact;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

/**
 * Binary operation fact
 */
public class BopFact extends ExpFact {

    /** Operator */
    private String operator;
    /** Expression 1 */
    private ExpFact firstExp;
    /** Expression 2 */
    private ExpFact secondExp;

    public BopFact(String operator, ExpFact firstExp, ExpFact secondExp) {
        this.operator = operator;
        this.firstExp = firstExp;
        this.secondExp = secondExp;
    }

    public String getOperator() {
        return operator;
    }

    public void setOperator(String operator) {
        this.operator = operator;
    }

    public ExpFact getFirstExp() {
        return firstExp;
    }

    public void setFirstExp(ExpFact firstExp) {
        this.firstExp = firstExp;
    }

    public ExpFact getSecondExp() {
        return secondExp;
    }

    public void setSecondExp(ExpFact secondExp) {
        this.secondExp = secondExp;
    }

    public List<Fact> toFactList() {
        List<Fact> factList = new ArrayList<>();
        factList.addAll(firstExp.toFactList());
        factList.addAll(secondExp.toFactList());
        factList.add(this);
        return factList;
    }

    @Override
    public String toString() {
        return String.format("(%s) %s (%s)", firstExp, operator, secondExp);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        BopFact bopFact = (BopFact) o;
        return Objects.equals(operator, bopFact.operator) && Objects.equals(firstExp, bopFact.firstExp) && Objects.equals(secondExp, bopFact.secondExp);
    }

    @Override
    public int hashCode() {
        return Objects.hash(operator, firstExp, secondExp);
    }

    @Override
    public List<ExpFact> getChildren() {
        return Arrays.asList(firstExp, secondExp);
    }

    @Override
    public void replace(ExpFact oldExp, ExpFact newExp) {
        if (firstExp.equals(oldExp)) {
            firstExp = newExp;
        }
        if (secondExp.equals(oldExp)) {
            secondExp = newExp;
        }
    }
}
