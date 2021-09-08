import Facts.Fact;
import Facts.exp.ExpFact;
import Facts.exp.MemFact;
import Facts.exp.VarFact;
import Facts.inst.EnterSubFact;
import Facts.inst.ExitSubFact;
import Facts.inst.NopFact;
import Facts.inst.ParamFact;
import Facts.inst.assign.LoadFact;
import Facts.inst.assign.MoveFact;
import Facts.inst.assign.StoreFact;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.TerminalNode;
import java.util.List;


public class StatementLoader implements BilListener {

    List<Fact> facts;

    public StatementLoader(List<Fact> lines) {
        this.facts = lines;
    }

    private ExpFact parseExpression(BilParser.ExpContext ctx) {
        if (ctx == null) {
            return null;
        }
        return ;
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

    }

    @Override
    public void enterProgdecl(BilParser.ProgdeclContext ctx) {

    }

    @Override
    public void exitProgdecl(BilParser.ProgdeclContext ctx) {

    }

    @Override
    public void enterSub(BilParser.SubContext ctx) {
        String address = ctx.addr().getText();
        String funcName = ctx.functionName().getText();
        facts.add(new EnterSubFact(address, funcName));
    }

    @Override
    public void exitSub(BilParser.SubContext ctx) {

    }

    @Override
    public void enterParamTypes(BilParser.ParamTypesContext ctx) {
        String address = ctx.addr().getText();
        String id = ctx.param().getText(); // human-readable name
        String variable = ctx.var().getText(); // some register, probably
        boolean isResult = id.contains("result");
        if (id.contains("result")) {
            facts.add(new ParamFact(address, new VarFact("out"), new VarFact(variable), true));
        } else {
            facts.add(new ParamFact(address, new VarFact(id), new VarFact(variable), false));
        }
    }

    @Override
    public void exitParamTypes(BilParser.ParamTypesContext ctx) {

    }

    @Override
    public void enterStmt(BilParser.StmtContext ctx) {
        String address = ctx.addr().getText();
        // statements can be assignments, jumps, conditional jumps or function calls
        if (ctx.assign() != null) {
            BilParser.AssignContext assignCtx = ctx.assign();
            if (assignCtx.exp().getClass().equals(BilParser.ExpLoadContext.class)) {
                BilParser.ExpLoadContext loadCtx = (BilParser.ExpLoadContext) assignCtx.exp();
                VarFact lhs = new VarFact(loadCtx.exp(1).getText());
                ExpFact rhs = parseExpression(loadCtx.exp(2));
                if (rhs != null) { // null check is necessary as rhs may not exist for loads
                    facts.add(new LoadFact(address, lhs, (MemFact) rhs));
                }
            } else if (assignCtx.exp().getClass().equals(BilParser.ExpStoreContext.class)) {
                BilParser.ExpStoreContext storeCtx = (BilParser.ExpStoreContext) assignCtx.exp();
                ExpFact lhs = parseExpression(storeCtx.exp(1));
                ExpFact rhs = parseExpression(storeCtx.exp(2));
                facts.add(new StoreFact(address, (MemFact) lhs, rhs));
            } else {
                VarFact lhs = new VarFact(assignCtx.var().getText());
                ExpFact rhs = parseExpression(assignCtx.exp());
                facts.add(new MoveFact(address, lhs, rhs));
            }
        } else if (ctx.jmp() != null) {

            handleJump(ctx.jmp());
        } else if (ctx.cjmp() != null) {
            handleCJump(ctx.cjmp());
        } else if (ctx.call() != null) {
            handleCall(ctx.call());
        } else {
            // this statement is empty
            facts.add(new NopFact(address));
        }


        String address = ctx.
                String.format("call %s(); goto label%s", ctx.functionName().getText(), ctx.returnaddr().addr().getText())
    }

    @Override
    public void exitStmt(BilParser.StmtContext ctx) {

    }

    @Override
    public void enterEndsub(BilParser.EndsubContext ctx) {
        String address = ctx.addr().getText();
        String funcName = ctx.functionName().getText();
        facts.add(new ExitSubFact(address, funcName));
    }

    @Override
    public void exitEndsub(BilParser.EndsubContext ctx) {

    }

    @Override
    public void enterCall(BilParser.CallContext ctx) {
    }

    @Override
    public void exitCall(BilParser.CallContext ctx) {

    }

    @Override
    public void enterAssign(BilParser.AssignContext assignCtx) {

    }

    @Override
    public void exitAssign(BilParser.AssignContext ctx) {

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

    }

    @Override
    public void enterJmp(BilParser.JmpContext ctx) {

    }

    @Override
    public void exitJmp(BilParser.JmpContext ctx) {

    }

    @Override
    public void enterVar(BilParser.VarContext ctx) {

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
    public void enterParam(BilParser.ParamContext ctx) {

    }

    @Override
    public void exitParam(BilParser.ParamContext ctx) {

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
    public void enterAddr(BilParser.AddrContext ctx) {

    }

    @Override
    public void exitAddr(BilParser.AddrContext ctx) {

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
