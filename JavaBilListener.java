
import Facts.Fact;
import Facts.SuccessorFact;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.ParseTreeListener;
import org.antlr.v4.runtime.tree.TerminalNode;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class JavaBilListener implements BilListener {
    private HashMap<String, String> functionStarts;
    private HashMap<String, String> functionEnds;
    public List<Fact> facts;

    public JavaBilListener() {
        functionStarts = new HashMap<>();
        functionEnds = new HashMap<>();
        facts = new ArrayList<>();
    }

    @Override
    public void enterBil(BilParser.BilContext ctx) {
    }

    @Override
    public void exitBil(BilParser.BilContext ctx) {

    }

    @Override
    public void enterBlock(BilParser.BlockContext ctx) {
        functionStarts.put(ctx.sub().ID(0).toString() ,ctx.stmt(0).NUMBER().toString());
        functionEnds.put(ctx.sub().ID(0).toString() , ctx.endsub().NUMBER().toString());

        List<BilParser.StmtContext> sctxs = ctx.stmt();
        int i;
        for (i = 0; i < sctxs.size() - 1; i++) {
            String i1 = sctxs.get(i).NUMBER().toString();

            String i2;
            if (sctxs.get(i).call() != null) {
                String fname = sctxs.get(i).call().ID().toString();
                i2 = functionStarts.get(fname);

                // Return call
                String j1 = functionEnds.get(fname);
                String j2 = sctxs.get(i).call().returnaddr().number().NUMBER().toString();
                Fact fact = new SuccessorFact(j1, j2);
                facts.add(fact);
            } else {
                i2 = sctxs.get(i + 1).NUMBER().toString();
            }
            Fact fact = new SuccessorFact(i1, i2);
            System.out.println(fact);
        }
        /* Last statement successor is the return call */
        String i1 = sctxs.get(i).NUMBER().toString();
        String i2 = ctx.endsub().NUMBER().toString();
        Fact fact = new SuccessorFact(i1, i2);
        facts.add(fact);
    }

    @Override
    public void exitBlock(BilParser.BlockContext ctx) {

    }

    @Override
    public void enterVar(BilParser.VarContext ctx) {

    }

    @Override
    public void exitVar(BilParser.VarContext ctx) {

    }

    @Override
    public void enterSub(BilParser.SubContext ctx) {

    }

    @Override
    public void exitSub(BilParser.SubContext ctx) {

    }

    @Override
    public void enterEndsub(BilParser.EndsubContext ctx) {

    }

    @Override
    public void exitEndsub(BilParser.EndsubContext ctx) {

    }

    @Override
    public void enterStmt(BilParser.StmtContext ctx) {

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
    public void enterWord(BilParser.WordContext ctx) {

    }

    @Override
    public void exitWord(BilParser.WordContext ctx) {

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
    public void enterNumber(BilParser.NumberContext ctx) {
    }

    @Override
    public void exitNumber(BilParser.NumberContext ctx) {

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