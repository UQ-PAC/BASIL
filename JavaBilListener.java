
import Facts.*;
import Facts.exp.*;
import Facts.inst.CjmpFact;
import Facts.inst.JmpFact;
import Facts.inst.assign.LoadFact;
import Facts.inst.assign.MoveFact;
import Facts.inst.assign.StoreFact;
import Facts.misc.IsRegFact;
import Facts.misc.SuccessorFact;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.TerminalNode;

import java.util.*;

public class JavaBilListener implements BilListener {
    /** First line (of the body) of each function */
    private HashMap<String, String> functionStarts;
    /** Return statements of each function*/
    private HashMap<String, Set<String>> functionEnds;
    /** List of generated Datalog facts */
    public Set<Fact> facts;
    /** Current program counter */
    private String currentPc;


    public JavaBilListener() {
        functionStarts = new HashMap<>();
        functionEnds = new HashMap<>();
        facts = new HashSet<>();
    }

    @Override
    public void enterBil(BilParser.BilContext ctx) {

    }

    @Override
    public void exitBil(BilParser.BilContext ctx) {

    }

    @Override
    public void enterFunction(BilParser.FunctionContext ctx) {

    }

    @Override
    public void exitFunction(BilParser.FunctionContext ctx) {
        String fname = ctx.sub().functionName().getText();

        /* Start of the function is first statement */
        functionStarts.put(fname, ctx.stmt(0).addr().getText());

        /* End of the function is the return statement */
        functionEnds.putIfAbsent(fname, new HashSet<>());
        functionEnds.get(fname).add(ctx.endsub().addr().getText());

        /* Create the successor function for all instructions in this function */
        int i;
        List<BilParser.StmtContext> statementCtx = ctx.stmt();
        for (i = 0; i < statementCtx.size() - 1; i++) {
            String i1 = statementCtx.get(i).addr().getText();
            String i2;
            if (statementCtx.get(i).call() != null) {
                /* If the statement is a call statement */
                String target = statementCtx.get(i).call().functionName().getText();
                i2 = functionStarts.get(target);

                /* The successor of the return statement in the target function is the return address */
                for (String j1 : functionEnds.get(target)) {
                    /* This is done for all return statements in the target function */
                    String j2 = statementCtx.get(i).call().returnaddr().addr().getText();
                    facts.add(new SuccessorFact(j1, j2));
                }
            } else {
                /* Otherwise the successor is simply the next line */
                i2 = statementCtx.get(i + 1).addr().getText();
            }
            facts.add(new SuccessorFact(i1, i2));
        }

        /* The last pair of statements in the function body (done separately) */
        String i1 = statementCtx.get(i).addr().getText();
        String i2 = ctx.endsub().addr().getText();
        facts.add(new SuccessorFact(i1, i2));
    }


    @Override
    public void enterVar(BilParser.VarContext ctx) {
        String varName = ctx.getText();
        facts.add(new IsRegFact(varName));
    }

    @Override
    public void exitVar(BilParser.VarContext ctx) {

    }

    @Override
    public void enterFunctionName(BilParser.FunctionNameContext ctx) {

    }

    @Override
    public void exitFunctionName(BilParser.FunctionNameContext ctx) {

    }

    @Override
    public void enterSub(BilParser.SubContext ctx) {

    }

    @Override
    public void exitSub(BilParser.SubContext ctx) {

    }

    @Override
    public void enterParamTypes(BilParser.ParamTypesContext ctx) {

    }

    @Override
    public void exitParamTypes(BilParser.ParamTypesContext ctx) {

    }

    @Override
    public void enterEndsub(BilParser.EndsubContext ctx) {

    }

    @Override
    public void exitEndsub(BilParser.EndsubContext ctx) {

    }

    @Override
    public void enterProgdecl(BilParser.ProgdeclContext ctx) {

    }

    @Override
    public void exitProgdecl(BilParser.ProgdeclContext ctx) {

    }

    @Override
    public void enterStmt(BilParser.StmtContext ctx) {
        currentPc = ctx.addr().getText();
    }


    @Override
    public void exitStmt(BilParser.StmtContext ctx) {

    }

    @Override
    public void enterParam(BilParser.ParamContext ctx) {

    }

    @Override
    public void exitParam(BilParser.ParamContext ctx) {

    }


    @Override
    public void enterAssign(BilParser.AssignContext assignCtx) {
        BilParser.ExpContext expCtx = assignCtx.exp();
        expCtx = ignoreUnhandledContexts(expCtx);

        if (expCtx.getClass().equals(BilParser.ExpLoadContext.class)) {
            /* Assignment is a load */
            BilParser.ExpLoadContext c = (BilParser.ExpLoadContext) expCtx;

            String lhs = assignCtx.var().getText();
            MemFact memFact = new MemFact(parseExpression(c.exp(1)));
            facts.add(new LoadFact(currentPc, lhs, memFact.id));
        } else if (expCtx.getClass().equals(BilParser.ExpStoreContext.class)) {
            /* Assignment is a store */
            BilParser.ExpStoreContext c = (BilParser.ExpStoreContext) expCtx;

            /* LHS of a store is an expression (IMPORTANT) */
            MemFact memFact = new MemFact(parseExpression(c.exp(1)));
            String rhs = parseExpression(c.exp(2));
            facts.add(new StoreFact(currentPc, memFact.id, rhs));
        } else {
            /* Assignment is a move */
            String lhs = assignCtx.var().getText();
            String rhs = parseExpression(expCtx);
            facts.add(new MoveFact(currentPc, lhs, rhs));
        }
    }

    /**
     * Skips all unhandled expression contexts.
     *
     * @param ectx an expression context
     * @return expression context that is not unhandled
     */
    private BilParser.ExpContext ignoreUnhandledContexts(BilParser.ExpContext ectx) {
        if (ectx.getClass().equals(BilParser.ExpCastContext.class)) {
            /* Ignore all casts */
            ectx = ((BilParser.ExpCastContext) ectx).exp();
            return ignoreUnhandledContexts(ectx);
        } else if (ectx.getClass().equals(BilParser.ExpExtractContext.class)) {
            /* Ignore all extracts */
            ectx = ((BilParser.ExpExtractContext) ectx).exp();
            return ignoreUnhandledContexts(ectx);
        } else {
            return ectx;
        }
    }

    /**
     * Creates and stores a given expression, and all sub-expressions in the facts list.
     *
     * NOTE: Memory expressions are handled separately in the assignment context.
     *
     * @param ectx expression context
     * @return unique expression identifier
     */
    private String parseExpression(BilParser.ExpContext ectx) {
        ExpFact expFact = null;
        ectx = ignoreUnhandledContexts(ectx);

        if (ectx.getClass().equals(BilParser.ExpBopContext.class)) {
            /* Binary operation expression */
            BilParser.ExpBopContext ctx = (BilParser.ExpBopContext) ectx;

            BilParser.ExpContext left = ctx.exp(0);
            BilParser.ExpContext right = ctx.exp(1);
            String op = ctx.bop().getText();
            expFact = new BopFact(op, parseExpression(left), parseExpression(right));
        } else if (ectx.getClass().equals(BilParser.ExpUopContext.class)) {
            /* Unary operation expression */
            BilParser.ExpUopContext ctx = (BilParser.ExpUopContext) ectx;

            BilParser.ExpContext exp = ctx.exp();
            String op = ctx.uop().getText();
            expFact = new UopFact(op, parseExpression(exp));
        } else if (ectx.getClass().equals(BilParser.ExpVarContext.class)) {
            /* Variable expression */
            BilParser.ExpVarContext ctx = (BilParser.ExpVarContext) ectx;

            expFact = new VarFact(ctx.var().getText());
        } else if (ectx.getClass().equals(BilParser.ExpLiteralContext.class)) {
            /* Literal expression */
            BilParser.ExpLiteralContext ctx = (BilParser.ExpLiteralContext) ectx;

            expFact = new LiteralFact(ctx.literal().getText());
        }
        if (expFact == null) {
            System.err.println("Unhandled expression detected: " + ectx.getText());
            return "";
        } else {
            facts.add(expFact);
            return expFact.id;
        }
    }

    @Override
    public void exitAssign(BilParser.AssignContext ctx) {

    }

    @Override
    public void enterCall(BilParser.CallContext ctx) {

    }

    @Override
    public void exitCall(BilParser.CallContext ctx) {

    }
    @Override
    public void enterExpBracket(BilParser.ExpBracketContext ctx) {
    }

    @Override
    public void exitExpBracket(BilParser.ExpBracketContext ctx) {

    }

    @Override
    public void enterExpUop(BilParser.ExpUopContext ctx) {
    }

    @Override
    public void exitExpUop(BilParser.ExpUopContext ctx) {

    }

    @Override
    public void enterExpVar(BilParser.ExpVarContext ctx) {

    }

    @Override
    public void exitExpVar(BilParser.ExpVarContext ctx) {

    }

    @Override
    public void enterExpLiteral(BilParser.ExpLiteralContext ctx) {
    }

    @Override
    public void exitExpLiteral(BilParser.ExpLiteralContext ctx) {

    }

    @Override
    public void enterExpExtract(BilParser.ExpExtractContext ctx) {

    }

    @Override
    public void exitExpExtract(BilParser.ExpExtractContext ctx) {

    }

    @Override
    public void enterExpCast(BilParser.ExpCastContext ctx) {

    }

    @Override
    public void exitExpCast(BilParser.ExpCastContext ctx) {

    }

    @Override
    public void enterExpLoad(BilParser.ExpLoadContext ctx) {

    }

    @Override
    public void exitExpLoad(BilParser.ExpLoadContext ctx) {

    }

    @Override
    public void enterExpStore(BilParser.ExpStoreContext ctx) {

    }

    @Override
    public void exitExpStore(BilParser.ExpStoreContext ctx) {

    }

    @Override
    public void enterExpBop(BilParser.ExpBopContext ctx) {
    }

    @Override
    public void exitExpBop(BilParser.ExpBopContext ctx) {

    }

    @Override
    public void enterCjmp(BilParser.CjmpContext ctx) {

    }

    @Override
    public void exitCjmp(BilParser.CjmpContext ctx) {
        String cond = ctx.var().getText();
        String target = ctx.addr().getText();
        facts.add(new CjmpFact(currentPc, target, cond));
    }

    @Override
    public void enterJmp(BilParser.JmpContext ctx) {
        String target = "";
        if (ctx.var() != null) {
            target = ctx.var().getText();
        } else if (ctx.addr() != null) {
            target = ctx.addr().getText();
        }
        facts.add(new JmpFact(currentPc, target));
    }

    @Override
    public void exitJmp(BilParser.JmpContext ctx) {

    }

    @Override
    public void enterBop(BilParser.BopContext ctx) {
    }

    @Override
    public void exitBop(BilParser.BopContext ctx) {

    }

    @Override
    public void enterUop(BilParser.UopContext ctx) {

    }

    @Override
    public void exitUop(BilParser.UopContext ctx) {

    }

    @Override
    public void enterInout(BilParser.InoutContext ctx) {

    }

    @Override
    public void exitInout(BilParser.InoutContext ctx) {

    }

    @Override
    public void enterReturnaddr(BilParser.ReturnaddrContext ctx) {

    }

    @Override
    public void exitReturnaddr(BilParser.ReturnaddrContext ctx) {

    }

    @Override
    public void enterAddr(BilParser.AddrContext ctx) {

    }

    @Override
    public void exitAddr(BilParser.AddrContext ctx) {

    }

    @Override
    public void enterLiteral(BilParser.LiteralContext ctx) {

    }

    @Override
    public void exitLiteral(BilParser.LiteralContext ctx) {

    }

    @Override
    public void enterNat(BilParser.NatContext ctx) {

    }

    @Override
    public void exitNat(BilParser.NatContext ctx) {

    }

    @Override
    public void visitTerminal(TerminalNode terminalNode) {

    }

    @Override
    public void visitErrorNode(ErrorNode errorNode) {

    }

    @Override
    public void enterEveryRule(ParserRuleContext parserRuleContext) {

    }

    @Override
    public void exitEveryRule(ParserRuleContext parserRuleContext) {

    }
}