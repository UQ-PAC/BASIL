import org.antlr.v4.runtime.ParserRuleContext;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.*;

public class BoogieTranslator {

    BufferedWriter writer;
    List<ParserRuleContext> lines;
    String funcName = "";
    int expectedParams = 0;
    Set<String> usedLabels = new HashSet<>();
    Map<String, Integer> registerValues = new HashMap<>();
    // the current index of 'lines' that is being analysed
    int lineIndex = 0;

    public BoogieTranslator(List<ParserRuleContext> lines, String outputFileName) {
        try {
            writer = new BufferedWriter(new FileWriter(outputFileName, false));
        } catch (IOException e) {
            System.err.println("Error setting up file writer.");
        }
        this.lines = lines;
    }

    public void translate() {
        logUsedLabels();
        handleInit();
        for (ParserRuleContext line : lines) {
            handleLine(line);
        }
    }

    private void logUsedLabels() {
        // label analysis
        // find all used labels
        for (ParserRuleContext line : lines) {
            String target = "";
            // labels are used in jumps, cjumps and calls
            if (line.getClass() == BilParser.StmtContext.class) {
                BilParser.StmtContext stmt = (BilParser.StmtContext) line;
                if (stmt.jmp() != null) {
                    BilParser.JmpContext jump = stmt.jmp();
                    if (jump.var() != null) {
                        target = jump.var().getText();
                    } else if (jump.addr() != null) {
                        target = jump.addr().getText();
                    }
                } else if (stmt.cjmp() != null) {
                    BilParser.CjmpContext jump = stmt.cjmp();
                    target = jump.addr().getText();
                } else if (stmt.call() != null) {
                    BilParser.CallContext jump = stmt.call(); // todo
                    target = jump.returnaddr().addr().getText();
                } else {
                    continue;
                }
                usedLabels.add(String.format("%s", target));
            }
        }
    }

    private void handleInit() {
        // registers[0] represents the value at X0
        writeToFile("const registers: [int] int;\n");
        // memory[0x00111] represents the value stored at memory address 0x00111
        // fixme: for now, only integers can be stored in memory
        writeToFile("const memory: [int] int;\n");
        writeToFile("var SP: int, FP: int, LR: int;\n");
        writeToFile("SP = 0;\n");
        writeToFile("FP = ?;\n");
        writeToFile("LR = ?;\n");
        writeToFile("\n");
    }

    private void handleLine(ParserRuleContext line) {
        // lines can be statements, functions declarations (subs) functions returns (end subs) or function parameters
        if (line.getClass() == BilParser.StmtContext.class) {
            handleStatement((BilParser.StmtContext) line);
        } else if (line.getClass() == BilParser.SubContext.class) {
            handleSub((BilParser.SubContext) line);
        } else if (line.getClass() == BilParser.EndsubContext.class) {
            handleEndSub((BilParser.EndsubContext) line);
        } else if (line.getClass() == BilParser.ParamTypesContext.class) {
            handleParam((BilParser.ParamTypesContext) line);
        } else {
            System.err.printf("Unhandled line: %s%n", line.getText());
        }
        // end every line with this
        writeToFile("\n");
        lineIndex++;
    }

    private void handleStatement(BilParser.StmtContext ctx) {
        // visual index for functions
        if (!funcName.equals("")) {
            writeToFile("    ");
        }
        // add label if it is used in the program
        if (usedLabels.contains(ctx.addr().getText())) {
            writeToFile("label" + ctx.addr().getText() + ": ");
        }
        // statements can be assignments, jumps, conditional jumps or function calls
        if (ctx.assign() != null) {
            handleAssignment(ctx.assign());
        } else if (ctx.jmp() != null) {
            handleJump(ctx.jmp());
        } else if (ctx.cjmp() != null) {
            handleCJump(ctx.cjmp());
        } else if (ctx.call() != null) {
            handleCall(ctx.call());
        } else {
            writeToFile("skip");
        }
        // end every statement with this
        writeToFile(";");
    }

    private void handleAssignment(BilParser.AssignContext ctx) {
        // assignments can be loads, stores or moves
        if (ctx.exp().getClass().equals(BilParser.ExpLoadContext.class)) {
            handleLoad((BilParser.ExpLoadContext) ctx.exp());
        } else if (ctx.exp().getClass().equals(BilParser.ExpStoreContext.class)) {
            handleStore((BilParser.ExpStoreContext) ctx.exp());
        } else {
            handleMove(ctx);
        }
    }

    private void handleLoad(BilParser.ExpLoadContext ctx) {
        // 'mem[x0,el]:u32'
        if (ctx.exp(2) == null) {
            writeToFile("skip");
            return; // no rhs of expression: this is likely a padding instruction on the variable c.exp(1), a.k.a. assignCtx.var()
        }
        // fixme; unsure this code will ever run
        String lhs = parseExpression(ctx.exp(1));
        String rhs = parseExpression(ctx.exp(2));
        writeToFile(String.format("%s := mem[%s]", lhs, rhs));
    }

    private void handleStore(BilParser.ExpStoreContext ctx) {
        // 'memwith[X0,el]:u32<-low:32[X1]'
        String lhs = parseExpression(ctx.exp(1)); // 'X0'
        String rhs = parseExpression(ctx.exp(2)); // 'X1', potentially removes casts such as 'low:32[X1]'
        writeToFile(String.format("mem[%s] := %s", evaluateExpression(ctx.exp(1), lineIndex), rhs)); // fixme
    }

    private void handleMove(BilParser.AssignContext ctx) {
        String lhs = ctx.var().getText();
        if (lhs.charAt(0) == 'X') {
            lhs = parseRegister(lhs);
        }
        String rhs = parseExpression(ctx.exp());
        writeToFile(String.format("%s := %s", lhs, rhs));
    }

    /*
    We have an expression a + b.
    Call evaluate expression(a + b).
    This returns evaluate expression(a) + evaluate expression(b).
    Evaluate expression(a) attempts to resolve a by searching backwards through the code.
    It increments back until it finds a non-statement, or an assignment statement with lhs === a.
    Once it finds this assignment statement, it calls evaluate expression on the rhs of the statement.
    Global line counters aren't touched, by local decrements/increments are passed to function calls.
    */
    // incomplete: only handles addition and subtraction of literals, and only evaluates move assignments
    // lineNo is the line this expression is on
    private Integer evaluateExpression(BilParser.ExpContext ectx, int lineNo) {
        if (ectx.getClass().equals(BilParser.ExpLiteralContext.class)) {
            return Integer.decode(ectx.getText());
        } else if (ectx.getClass().equals(BilParser.ExpBopContext.class)) {
            BilParser.ExpBopContext ctx = (BilParser.ExpBopContext) ectx;
            Integer lhs = evaluateExpression(ctx.exp(0), lineNo);
            Integer rhs = evaluateExpression(ctx.exp(1), lineNo);
            if (lhs != null && rhs != null) {
                if (ctx.bop().PLUS() != null) {
                    return lhs + rhs;
                } else if (ctx.bop().MINUS() != null) {
                    return lhs - rhs;
                }
            }
        } else if (ectx.getClass().equals(BilParser.ExpVarContext.class)) {
            String var = ectx.getText();
            int lineOffset = 0;
            ParserRuleContext line;
            while ((line = lines.get(lineNo - (++lineOffset))).getClass().equals(BilParser.StmtContext.class)) {
                BilParser.StmtContext stmt = (BilParser.StmtContext) line;
                if (stmt.assign() == null) {
                    continue; // not an assign statement
                }
                BilParser.AssignContext assignment = stmt.assign();
                if (assignment.exp().getClass().equals(BilParser.ExpLoadContext.class) ||
                        assignment.exp().getClass().equals(BilParser.ExpStoreContext.class)) {
                    // we don't evaluate non-move assignments yet. todo
                    System.err.println("Can't guarantee accuracy of variable substitution as a non-move assignment was encountered");
                    continue;
                }
                if (!assignment.var().getText().equals(var)) {
                    continue; // this assign statement doesn't update the variable we're evaluating
                }
                return evaluateExpression(assignment.exp(), lineNo - lineOffset);
            } // reached a non-statement
        }
        System.err.printf("Failed to evaluate expression: %s%n", ectx.getText());
        return null;
    }

    private void handleJump(BilParser.JmpContext ctx) {
        String target = "";
        if (ctx.var() != null) {
            target = ctx.var().getText();
        } else if (ctx.addr() != null) {
            target = ctx.addr().getText();
        }
        writeToFile(String.format("goto label%s", target));
    }

    private void handleCJump(BilParser.CjmpContext ctx) {
        String cond = ctx.var().getText();
        String target = ctx.addr().getText();
        writeToFile(String.format("if (%s) {goto label%s}", cond, target));
    }

    private void handleCall(BilParser.CallContext ctx) {
        // fixme: bill parser is having trouble understanding when we reach the end of a function call, and sometimes throws a null pointer exception if it doesn't think the end was reached
        try {
            writeToFile(String.format("call %s(); goto label%s", ctx.functionName().getText(), ctx.returnaddr().addr().getText()));
        } catch (NullPointerException e) {
            System.err.println("ignored null pointer exception");
        }
    }

    private void handleSub(BilParser.SubContext ctx) {
        String functionName = ctx.functionName().getText();
        List<BilParser.ParamContext> params = ctx.param();
        expectedParams = params.size();
        // at this point, we just assume that the registers and memory are modified by every function
        // this can be cleaned up on a second parsing
        writeToFile(String.format("procedure %s()%n    modifies mem%n    modifies registers", functionName));
        funcName = functionName;
    }

    private void handleEndSub(BilParser.EndsubContext ctx) {
        writeToFile(String.format("}%n"));
        funcName = "";
    }

    private void handleParam(BilParser.ParamTypesContext ctx) {
        String id = ctx.param().getText();
        if (id.contains("result")) {
            id = "@result:";
        } else {
            id = id.replace(funcName + "_", "");
            id = "@param " + id + ":";
        }
        String inOut = ctx.inout().getText();
        if (inOut.equals("out")) {
            inOut = "to";
        } else {
            inOut = "from"; // warning: also includes 'in and out' (i.e. as well as just 'in')
        }
        String variable = ctx.var().getText();
        if (variable.charAt(0) == 'X') {
            variable = parseRegister(variable); // fixme warning: assumes all registers start with 'X' and no other variable does
        }
        writeToFile(String.format("    %s %s %s", id, inOut, variable));
        if (--expectedParams == 0) {
            writeToFile("\n{");
        }
    }

    private String parseExpression(BilParser.ExpContext ectx) {
        // expressions can be binary operations, unary operations, variables, literals or extractions (i.e. bit slices)
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
            System.err.println("Unhandled expression detected: " + ectx.getText());
            return "";
        }

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

    private String parseRegister(String registerID) {
        return String.format("registers[%d]", Integer.parseInt(registerID.substring(1)));
    }

    private void writeToFile(String text) {
        try {
            writer.write(text);
            writer.flush();
        } catch (IOException e) {
            System.err.println("Error writing to file.");
        }
    }
}
