package facts.exp;

import facts.Fact;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

/**
 * Binary operation fact
 */
public class BinOp extends Expr {

    /** Operator */
    private String operator;
    /** Expression 1 */
    private Expr firstExp;
    /** Expression 2 */
    private Expr secondExp;

    public BinOp(String operator, Expr firstExp, Expr secondExp) {
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

    public Expr getFirstExp() {
        return firstExp;
    }

    public void setFirstExp(Expr firstExp) {
        this.firstExp = firstExp;
    }

    public Expr getSecondExp() {
        return secondExp;
    }

    public void setSecondExp(Expr secondExp) {
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
        BinOp binOp = (BinOp) o;
        return Objects.equals(operator, binOp.operator) && Objects.equals(firstExp, binOp.firstExp) && Objects.equals(secondExp, binOp.secondExp);
    }

    @Override
    public int hashCode() {
        return Objects.hash(operator, firstExp, secondExp);
    }

    @Override
    public List<Expr> getChildren() {
        return Arrays.asList(firstExp, secondExp);
    }

    @Override
    public void replace(Expr oldExp, Expr newExp) {
        if (firstExp.equals(oldExp)) {
            firstExp = newExp;
        }
        if (secondExp.equals(oldExp)) {
            secondExp = newExp;
        }
    }
}
