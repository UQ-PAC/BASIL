import Facts.Fact;
import Facts.exp.*;
import Facts.inst.CjmpFact;
import Facts.inst.JmpFact;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.TerminalNode;
import java.util.*;
import java.util.regex.Pattern;

/** Notes
 * Right now, we're only indexing one level deep on assignment expressions, but lhs and rhs might be more complex than
 * single variables.
 * In future, we should look at recursively indexing expressions down to the variable or literal level.
 *
 * Anything assigned to a hex value of some sort should probably be assigned a bit vector type, as it probably
 * represents some memory address, on which functions like extract could be used.
 * These extract functions should be encoded in boogie as native bit-vector slices (no external functions needed).
 * "v := extract:x:y[z]" ==> "v := z[v.size-x:v.size-y-1];" where v.size = number of bits in v
 *
 * Make each pc a label:
 * "{pc} {stmt (optional)}" ==> "lab{pc}: {stmt};"
 *
 * Now we can support gotos:
 * "goto %{pc}" ==> "goto lab{pc};"
 *
 * todo: test code coverage
 */

public class BoogieBillListener implements BilListener {

    @Override
    public void enterBil(BilParser.BilContext ctx) {
        System.out.println("const registers: [int] int;"); // registers[0] represents the value at X0
        System.out.println("const memory: [int] int;"); // memory[0x00111] represents the value stored at memory address 0x00111. fixme: for now, only integers can be stored in memory
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
    public void enterStmt(BilParser.StmtContext ctx) {
        String currentPc = ctx.addr().getText();
        System.out.print("label_" + currentPc + ": ");
        if (ctx.call() == null && ctx.assign() == null && ctx.cjmp() == null && ctx.jmp() == null) {
            System.out.print("skip");
        }
    }

    @Override
    public void exitStmt(BilParser.StmtContext ctx) {
        System.out.println(";");
    }

    @Override
    public void enterEndsub(BilParser.EndsubContext ctx) {

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
        BilParser.ExpContext expCtx = assignCtx.exp();
        expCtx = ignoreUnhandledContexts(expCtx);

        if (expCtx.getClass().equals(BilParser.ExpLoadContext.class)) {
            /* Assignment is a load */
            BilParser.ExpLoadContext c = (BilParser.ExpLoadContext) expCtx; // 'mem[x0,el]:u32'
            if (c.exp(2) == null) {
                System.out.print("skip");
                return; // no rhs of expression: this is likely a padding instruction on the variable c.exp(1), a.k.a. assignCtx.var()
            }
            // fixme; unsure this code will ever run
            String lhs = parseExpression(c.exp(1));
            String rhs = parseExpression(c.exp(2));
            System.out.printf("%s := mem[%s]", lhs, rhs);

        } else if (expCtx.getClass().equals(BilParser.ExpStoreContext.class)) {
            /* Assignment is a store */
            BilParser.ExpStoreContext c = (BilParser.ExpStoreContext) expCtx; // 'memwith[X0,el]:u32<-low:32[X1]'
            String lhs = parseExpression(c.exp(1)); // 'X0'
            String rhs = parseExpression(c.exp(2)); // 'X1', potentially removes casts such as 'low:32[X1]'
            System.out.printf("mem[%s] := %s", lhs, rhs);

        } else {
            /* Assignment is a move */
            String lhs = assignCtx.var().getText();
            if (lhs.charAt(0) == 'X') {
                lhs = parseRegister(lhs);
            }
            String rhs = parseExpression(expCtx);
            System.out.printf("%s := %s", lhs, rhs);
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
        ectx = ignoreUnhandledContexts(ectx);

        if (ectx.getClass().equals(BilParser.ExpBopContext.class)) {
            /* Binary operation expression */
            BilParser.ExpBopContext ctx = (BilParser.ExpBopContext) ectx;
            String left = parseExpression(ctx.exp(0));
            String right = parseExpression(ctx.exp(1));
            String op = ctx.bop().getText();
            if (op.equals("=")) {
                op = "=="; // a little patching here, a little there...
            }
            return String.format("(%s) %s (%s)", left, op, right);

        } else if (ectx.getClass().equals(BilParser.ExpUopContext.class)) {
            /* Unary operation expression */
            BilParser.ExpUopContext ctx = (BilParser.ExpUopContext) ectx;
            String exp = parseExpression(ctx.exp());
            String op = ctx.uop().getText();

            return String.format("%s (%s)", op, exp);
        } else if (ectx.getClass().equals(BilParser.ExpVarContext.class)) {
            /* Variable expression */
            BilParser.ExpVarContext ctx = (BilParser.ExpVarContext) ectx;
            // vars can be flags, registers (X[num]) or "expression bundles" (#[num]) where [num] is some integer
            String variable = ctx.var().getText();
            if (variable.charAt(0) == 'X') {
                // variable is a register, replace with call to register map
                return parseRegister(variable);
            } else if (variable.charAt(0) == '#') {
                // variable is an expression bundle todo
                return variable;
            } else {
                return variable;
            }

        } else if (ectx.getClass().equals(BilParser.ExpLiteralContext.class)) {
            /* Literal expression */
            BilParser.ExpLiteralContext ctx = (BilParser.ExpLiteralContext) ectx;
            return ctx.literal().getText();

        } else if (ectx.getClass().equals(BilParser.ExpExtractContext.class)) {
            /* Extraction expression */
            BilParser.ExpExtractContext ctx = (BilParser.ExpExtractContext) ectx;
            int firstNat = Integer.parseInt(ctx.nat(0).getText());
            int secondNat = Integer.parseInt(ctx.nat(1).getText());
            String exp = parseExpression(ctx.exp());
            // fixme: big warning! this is broken! assumes all bit vectors are 64 bits long
            // wasn't sure how to get the length of a bit vector: might have to get it from somewhere else in the program and keep track of a variable
            return String.format("%s[%d:%d]", exp, 64-firstNat, 64-secondNat-1); // fixme: in future, we want to properly translate exp before jamming it in here

        } else {
            System.err.print("Unhandled expression detected: " + ectx.getText());
            return "";
        }
    }

    private String parseRegister(String registerID) {
        return String.format("registers[%d]", Integer.parseInt(registerID.substring(1)));
    }

    /**
     * Skips all unhandled expression contexts.
     *
     * Unhandled expressions are:
     * - Casts
     *
     * @param ectx an expression context
     * @return expression context that is not unhandled
     */
    private BilParser.ExpContext ignoreUnhandledContexts(BilParser.ExpContext ectx) {
        if (ectx.getClass().equals(BilParser.ExpCastContext.class)) {
            /* Ignore all casts */
            ectx = ((BilParser.ExpCastContext) ectx).exp();
            return ignoreUnhandledContexts(ectx);
        } else {
            return ectx;
        }
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
        String cond = ctx.var().getText();
        String target = ctx.addr().getText();
        System.out.printf("if (%s) {goto label_%s}", cond, target);
    }

    @Override
    public void enterJmp(BilParser.JmpContext ctx) {
        String target = "";
        if (ctx.var() != null) {
            target = ctx.var().getText();
        } else if (ctx.addr() != null) {
            target = ctx.addr().getText();
        }
        System.out.printf("goto label_%s", target);
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
