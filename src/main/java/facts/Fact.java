package facts;

import astnodes.exp.Expr;
import java.util.List;

// TODO remove this (any reference to fact can really be a reference to stmt or exp)
public abstract class Fact {

    public abstract List<Expr> getChildren();

    // TODO is this necassary for all expresions or only Vars
    public abstract void replace(Expr oldExp, Expr newExp);
}
