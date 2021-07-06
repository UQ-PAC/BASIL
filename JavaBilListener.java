
import Facts.*;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.ParseTreeListener;
import org.antlr.v4.runtime.tree.TerminalNode;

import java.util.*;

public class JavaBilListener implements BilListener {
    private HashMap<String, String> functionStarts;
    private HashMap<String, String> functionEnds;
    public Set<Fact> facts;

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
    public void enterBlock(BilParser.BlockContext ctx) {
        String fname = ctx.sub().functionName().getText();
        functionStarts.put(fname, ctx.sub().addr().getText());
        functionEnds.put(fname, ctx.endsub().addr().getText());
        List<BilParser.StmtContext> statementCtx = ctx.stmt();
        int i;
        for (i = 0; i < statementCtx.size() - 1; i++) {
            String i1 = statementCtx.get(i).addr().getText();
            String i2;
            if (statementCtx.get(i).call() != null) {
                String target = statementCtx.get(i).call().functionName().getText();
                i2 = functionStarts.get(target);

                /* The successor of the return statement in the target function is the return address */
                String j1 = functionEnds.get(target);
                String j2 = statementCtx.get(i).call().returnaddr().addr().getText();
                facts.add(new SuccessorFact(j1, j2));
            } else {
                i2 = statementCtx.get(i + 1).addr().getText();
            }
            facts.add(new SuccessorFact(i1, i2));
        }
        /* Last statement successor is the return call */
        String i1 = statementCtx.get(i).addr().getText();
        String i2 = ctx.endsub().addr().getText();
        facts.add(new SuccessorFact(i1, i2));
    }

    @Override
    public void exitBlock(BilParser.BlockContext ctx) {

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
    public void enterStmt(BilParser.StmtContext ctx) {
        String pc = ctx.addr().getText();
        if (ctx.assign() != null) {
            BilParser.AssignContext assignCtx = ctx.assign();

            String lhs = assignCtx.var().getText();

            BilParser.ExpContext expCtx = assignCtx.exp();
            if (expCtx.bop() != null) {
                String bop = expCtx.bop().getText();
                String rhs1 = expCtx.exp(0).getText();
                String rhs2 = expCtx.exp(1).getText();
                facts.add(new BopFact(pc, lhs, rhs1, rhs2, bop));
            } else if (expCtx.uop() != null) {
                String uop = expCtx.uop().getText();
                String rhs = expCtx.exp(0).getText();
                facts.add(new UopFact(pc, lhs, rhs, uop));
            } else if (expCtx.var() != null) {
                String rhs = expCtx.var().getText();
                facts.add(new MoveFact(pc, lhs, rhs));
            }
        }


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
    public void enterAssign(BilParser.AssignContext ctx) {
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
    public void enterExp(BilParser.ExpContext ctx) {

    }

    @Override
    public void exitExp(BilParser.ExpContext ctx) {

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