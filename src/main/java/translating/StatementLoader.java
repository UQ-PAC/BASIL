package translating;

import facts.ExtractFact;
import facts.parameters.InParameter;
import facts.parameters.OutParameter;
import facts.exp.*;
import facts.inst.*;
import facts.inst.Assign.LoadFact;
import facts.inst.Assign.MoveFact;
import facts.inst.Assign.StoreFact;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.TerminalNode;
import java.util.List;
import BilParser.*;


public class StatementLoader implements BilListener {

    // list of facts to output
    List<InstFact> facts;
    // the last function header parsed; needed for assigning facts.parameters
    EnterSubFact currentFunction;
    // for generating unique labels
    int pcCount = 0;

    public StatementLoader(List<InstFact> facts) {
        this.facts = facts;
    }

    private ExpFact parseExpression(BilParser.ExpContext ctx) {
        if (ctx == null) {
            return null;
        } else if (ctx.getClass().equals(BilParser.ExpBopContext.class)) {
            return parseBinaryOperation((BilParser.ExpBopContext) ctx);
        } else if (ctx.getClass().equals(BilParser.ExpUopContext.class)) {
            return parseUnaryOperation((BilParser.ExpUopContext) ctx);
        } else if (ctx.getClass().equals(BilParser.ExpVarContext.class)) {
            return parseVariableExpression((BilParser.ExpVarContext) ctx);
        } else if (ctx.getClass().equals(BilParser.ExpLiteralContext.class)) {
            return parseLiteralExpression((BilParser.ExpLiteralContext) ctx);
        } else if (ctx.getClass().equals(BilParser.ExpExtractContext.class)) {
            return parseExtractionExpression((BilParser.ExpExtractContext) ctx);
        } else if (ctx.getClass().equals(BilParser.ExpCastContext.class)) {
            return parseCastExpression((BilParser.ExpCastContext) ctx);
        } else if (ctx.getClass().equals(BilParser.ExpLoadContext.class)) {
            // toReturn == null when this load is not a mem expression, which is currently not expected
            ExpFact toReturn = parseLoadExpression((BilParser.ExpLoadContext) ctx);
            if (toReturn != null) {
                return toReturn;
            }
        } else if (ctx.getClass().equals(BilParser.ExpBracketContext.class)) {
            return parseBracketExpression((BilParser.ExpBracketContext) ctx);
        }
        System.err.println("Unhandled expression detected: " + ctx.getText());
        return null;
    }

    private ExpFact parseBracketExpression(BilParser.ExpBracketContext ctx) {
        return parseExpression(ctx.exp());
    }

    private BopFact parseBinaryOperation(BilParser.ExpBopContext ctx) {
        ExpFact left = parseExpression(ctx.exp(0));
        ExpFact right = parseExpression(ctx.exp(1));
        String op = ctx.bop().getText();
        if (op.equals("=")) {
            op = "=="; // a little patching here, a little there...
        }
        return new BopFact(op, left, right);
    }

    private UopFact parseUnaryOperation(BilParser.ExpUopContext ctx) {
        ExpFact exp = parseExpression(ctx.exp());
        String op = ctx.uop().getText();
        return new UopFact(op, exp);
    }

    private VarFact parseVariableExpression(BilParser.ExpVarContext ctx) {
        return new VarFact(ctx.var().getText());
    }

    private LiteralFact parseLiteralExpression(BilParser.ExpLiteralContext ctx) {
        return new LiteralFact(ctx.literal().getText());
    }

    // fixme: assumes all bit vectors are 64 bits long
    private ExtractFact parseExtractionExpression(BilParser.ExpExtractContext ctx) {
        int firstNat = 64 - Integer.parseInt(ctx.nat(0).getText());
        int secondNat = 63 - Integer.parseInt(ctx.nat(1).getText());
        ExpFact exp = parseExpression(ctx.exp());
        return new ExtractFact(firstNat, secondNat, exp);
    }

    private ExpFact parseCastExpression(BilParser.ExpCastContext ctx) {
        // simply unwrap and throw away casts
        return parseExpression(ctx.exp());
    }

    private MemFact parseLoadExpression(BilParser.ExpLoadContext ctx) {
        if (ctx.exp(0).getText().equals("mem")) {
            return new MemFact(parseExpression(ctx.exp(1)));
        } else {
            return null;
        }
    }

    private String uniquePc() {
        int pc = pcCount++;
        return "l" + pc;
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
        String name = ctx.functionName().getText();
        EnterSubFact function = new EnterSubFact(address, name);
        facts.add(function);

        this.currentFunction = function;
    }

    @Override
    public void exitSub(BilParser.SubContext ctx) {

    }

    @Override
    public void enterParamTypes(BilParser.ParamTypesContext ctx) {
        String id = ctx.param().getText(); // human-readable name
        String variable = ctx.var().getText(); // some register, probably
        if (id.contains("result")) {
            currentFunction.setOutParam(new OutParameter(new VarFact("out"), new VarFact(variable)));
        } else {
            currentFunction.getInParams().add(new InParameter(new VarFact(id), new VarFact(variable)));
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
            // statement is assignment; check which type
            BilParser.AssignContext assignCtx = ctx.assign();
            if (assignCtx.exp().getClass().equals(BilParser.ExpLoadContext.class)) {
                // statement is a load assignment
                BilParser.ExpLoadContext loadCtx = (BilParser.ExpLoadContext) assignCtx.exp();
                VarFact lhs = new VarFact(loadCtx.exp(1).getText());
                ExpFact rhs = parseExpression(loadCtx.exp(2));
                if (rhs != null) { // null check is necessary as rhs may not exist for loads
                    facts.add(new LoadFact(address, lhs, (MemFact) rhs));
                }
            } else if (assignCtx.exp().getClass().equals(BilParser.ExpStoreContext.class)) {
                // statement is a store assignment
                BilParser.ExpStoreContext storeCtx = (BilParser.ExpStoreContext) assignCtx.exp();
                MemFact lhs = new MemFact(parseExpression(storeCtx.exp(1)));
                ExpFact rhs = parseExpression(storeCtx.exp(2));
                facts.add(new StoreFact(address, lhs, rhs));
            } else {
                // statement is a move assignment
                VarFact lhs = new VarFact(assignCtx.var().getText());
                ExpFact rhs = parseExpression(assignCtx.exp());
                facts.add(new MoveFact(address, lhs, rhs));
            }
        } else if (ctx.jmp() != null) {
            // statement is a jump
            String target = "";
            if (ctx.jmp().var() != null) {
                target = ctx.jmp().var().getText();
            } else if (ctx.jmp().addr() != null) {
                target = ctx.jmp().addr().getText();
            }
            facts.add(new JmpFact(address, target));
        } else if (ctx.cjmp() != null) {
            // statement is a conditional jump
            VarFact cond = new VarFact(ctx.cjmp().var().getText()); // conditions are always vars
            String target = ctx.cjmp().addr().getText();
            facts.add(new CjmpFact(address, target, cond));
        } else if (ctx.call() != null) {
            // statement is a call
            System.out.println(ctx.addr().getText());
            if (ctx.call().functionName() == null) {
                // occasionally this occurs with "call LR with no return" lines
                facts.add(new ExitSubFact(ctx.addr().getText()));
            } else {
                String funcName = ctx.call().functionName().getText();
                String returnAddr = ctx.call().returnaddr().addr().getText();
                facts.add(new CallFact(address, funcName));
                facts.add(new JmpFact(uniquePc(), returnAddr));
            }
        } else {
            // this statement is empty
            facts.add(new NopFact(address));
        }
    }

    @Override
    public void exitStmt(BilParser.StmtContext ctx) {

    }

    @Override
    public void enterEndsub(BilParser.EndsubContext ctx) {
        String address = ctx.addr().getText();
        facts.add(new ExitSubFact(address));
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
