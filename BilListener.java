// Generated from Bil.g4 by ANTLR 4.9.2
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link BilParser}.
 */
public interface BilListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link BilParser#bil}.
	 * @param ctx the parse tree
	 */
	void enterBil(BilParser.BilContext ctx);
	/**
	 * Exit a parse tree produced by {@link BilParser#bil}.
	 * @param ctx the parse tree
	 */
	void exitBil(BilParser.BilContext ctx);
	/**
	 * Enter a parse tree produced by {@link BilParser#function}.
	 * @param ctx the parse tree
	 */
	void enterFunction(BilParser.FunctionContext ctx);
	/**
	 * Exit a parse tree produced by {@link BilParser#function}.
	 * @param ctx the parse tree
	 */
	void exitFunction(BilParser.FunctionContext ctx);
	/**
	 * Enter a parse tree produced by {@link BilParser#progdecl}.
	 * @param ctx the parse tree
	 */
	void enterProgdecl(BilParser.ProgdeclContext ctx);
	/**
	 * Exit a parse tree produced by {@link BilParser#progdecl}.
	 * @param ctx the parse tree
	 */
	void exitProgdecl(BilParser.ProgdeclContext ctx);
	/**
	 * Enter a parse tree produced by {@link BilParser#sub}.
	 * @param ctx the parse tree
	 */
	void enterSub(BilParser.SubContext ctx);
	/**
	 * Exit a parse tree produced by {@link BilParser#sub}.
	 * @param ctx the parse tree
	 */
	void exitSub(BilParser.SubContext ctx);
	/**
	 * Enter a parse tree produced by {@link BilParser#paramTypes}.
	 * @param ctx the parse tree
	 */
	void enterParamTypes(BilParser.ParamTypesContext ctx);
	/**
	 * Exit a parse tree produced by {@link BilParser#paramTypes}.
	 * @param ctx the parse tree
	 */
	void exitParamTypes(BilParser.ParamTypesContext ctx);
	/**
	 * Enter a parse tree produced by {@link BilParser#stmt}.
	 * @param ctx the parse tree
	 */
	void enterStmt(BilParser.StmtContext ctx);
	/**
	 * Exit a parse tree produced by {@link BilParser#stmt}.
	 * @param ctx the parse tree
	 */
	void exitStmt(BilParser.StmtContext ctx);
	/**
	 * Enter a parse tree produced by {@link BilParser#endsub}.
	 * @param ctx the parse tree
	 */
	void enterEndsub(BilParser.EndsubContext ctx);
	/**
	 * Exit a parse tree produced by {@link BilParser#endsub}.
	 * @param ctx the parse tree
	 */
	void exitEndsub(BilParser.EndsubContext ctx);
	/**
	 * Enter a parse tree produced by {@link BilParser#call}.
	 * @param ctx the parse tree
	 */
	void enterCall(BilParser.CallContext ctx);
	/**
	 * Exit a parse tree produced by {@link BilParser#call}.
	 * @param ctx the parse tree
	 */
	void exitCall(BilParser.CallContext ctx);
	/**
	 * Enter a parse tree produced by {@link BilParser#assign}.
	 * @param ctx the parse tree
	 */
	void enterAssign(BilParser.AssignContext ctx);
	/**
	 * Exit a parse tree produced by {@link BilParser#assign}.
	 * @param ctx the parse tree
	 */
	void exitAssign(BilParser.AssignContext ctx);
	/**
	 * Enter a parse tree produced by the {@code expBracket}
	 * labeled alternative in {@link BilParser#exp}.
	 * @param ctx the parse tree
	 */
	void enterExpBracket(BilParser.ExpBracketContext ctx);
	/**
	 * Exit a parse tree produced by the {@code expBracket}
	 * labeled alternative in {@link BilParser#exp}.
	 * @param ctx the parse tree
	 */
	void exitExpBracket(BilParser.ExpBracketContext ctx);
	/**
	 * Enter a parse tree produced by the {@code expUop}
	 * labeled alternative in {@link BilParser#exp}.
	 * @param ctx the parse tree
	 */
	void enterExpUop(BilParser.ExpUopContext ctx);
	/**
	 * Exit a parse tree produced by the {@code expUop}
	 * labeled alternative in {@link BilParser#exp}.
	 * @param ctx the parse tree
	 */
	void exitExpUop(BilParser.ExpUopContext ctx);
	/**
	 * Enter a parse tree produced by the {@code expVar}
	 * labeled alternative in {@link BilParser#exp}.
	 * @param ctx the parse tree
	 */
	void enterExpVar(BilParser.ExpVarContext ctx);
	/**
	 * Exit a parse tree produced by the {@code expVar}
	 * labeled alternative in {@link BilParser#exp}.
	 * @param ctx the parse tree
	 */
	void exitExpVar(BilParser.ExpVarContext ctx);
	/**
	 * Enter a parse tree produced by the {@code expLiteral}
	 * labeled alternative in {@link BilParser#exp}.
	 * @param ctx the parse tree
	 */
	void enterExpLiteral(BilParser.ExpLiteralContext ctx);
	/**
	 * Exit a parse tree produced by the {@code expLiteral}
	 * labeled alternative in {@link BilParser#exp}.
	 * @param ctx the parse tree
	 */
	void exitExpLiteral(BilParser.ExpLiteralContext ctx);
	/**
	 * Enter a parse tree produced by the {@code expExtract}
	 * labeled alternative in {@link BilParser#exp}.
	 * @param ctx the parse tree
	 */
	void enterExpExtract(BilParser.ExpExtractContext ctx);
	/**
	 * Exit a parse tree produced by the {@code expExtract}
	 * labeled alternative in {@link BilParser#exp}.
	 * @param ctx the parse tree
	 */
	void exitExpExtract(BilParser.ExpExtractContext ctx);
	/**
	 * Enter a parse tree produced by the {@code expCast}
	 * labeled alternative in {@link BilParser#exp}.
	 * @param ctx the parse tree
	 */
	void enterExpCast(BilParser.ExpCastContext ctx);
	/**
	 * Exit a parse tree produced by the {@code expCast}
	 * labeled alternative in {@link BilParser#exp}.
	 * @param ctx the parse tree
	 */
	void exitExpCast(BilParser.ExpCastContext ctx);
	/**
	 * Enter a parse tree produced by the {@code expLoad}
	 * labeled alternative in {@link BilParser#exp}.
	 * @param ctx the parse tree
	 */
	void enterExpLoad(BilParser.ExpLoadContext ctx);
	/**
	 * Exit a parse tree produced by the {@code expLoad}
	 * labeled alternative in {@link BilParser#exp}.
	 * @param ctx the parse tree
	 */
	void exitExpLoad(BilParser.ExpLoadContext ctx);
	/**
	 * Enter a parse tree produced by the {@code expStore}
	 * labeled alternative in {@link BilParser#exp}.
	 * @param ctx the parse tree
	 */
	void enterExpStore(BilParser.ExpStoreContext ctx);
	/**
	 * Exit a parse tree produced by the {@code expStore}
	 * labeled alternative in {@link BilParser#exp}.
	 * @param ctx the parse tree
	 */
	void exitExpStore(BilParser.ExpStoreContext ctx);
	/**
	 * Enter a parse tree produced by the {@code expBop}
	 * labeled alternative in {@link BilParser#exp}.
	 * @param ctx the parse tree
	 */
	void enterExpBop(BilParser.ExpBopContext ctx);
	/**
	 * Exit a parse tree produced by the {@code expBop}
	 * labeled alternative in {@link BilParser#exp}.
	 * @param ctx the parse tree
	 */
	void exitExpBop(BilParser.ExpBopContext ctx);
	/**
	 * Enter a parse tree produced by {@link BilParser#cjmp}.
	 * @param ctx the parse tree
	 */
	void enterCjmp(BilParser.CjmpContext ctx);
	/**
	 * Exit a parse tree produced by {@link BilParser#cjmp}.
	 * @param ctx the parse tree
	 */
	void exitCjmp(BilParser.CjmpContext ctx);
	/**
	 * Enter a parse tree produced by {@link BilParser#jmp}.
	 * @param ctx the parse tree
	 */
	void enterJmp(BilParser.JmpContext ctx);
	/**
	 * Exit a parse tree produced by {@link BilParser#jmp}.
	 * @param ctx the parse tree
	 */
	void exitJmp(BilParser.JmpContext ctx);
	/**
	 * Enter a parse tree produced by {@link BilParser#var}.
	 * @param ctx the parse tree
	 */
	void enterVar(BilParser.VarContext ctx);
	/**
	 * Exit a parse tree produced by {@link BilParser#var}.
	 * @param ctx the parse tree
	 */
	void exitVar(BilParser.VarContext ctx);
	/**
	 * Enter a parse tree produced by {@link BilParser#functionName}.
	 * @param ctx the parse tree
	 */
	void enterFunctionName(BilParser.FunctionNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link BilParser#functionName}.
	 * @param ctx the parse tree
	 */
	void exitFunctionName(BilParser.FunctionNameContext ctx);
	/**
	 * Enter a parse tree produced by {@link BilParser#param}.
	 * @param ctx the parse tree
	 */
	void enterParam(BilParser.ParamContext ctx);
	/**
	 * Exit a parse tree produced by {@link BilParser#param}.
	 * @param ctx the parse tree
	 */
	void exitParam(BilParser.ParamContext ctx);
	/**
	 * Enter a parse tree produced by {@link BilParser#bop}.
	 * @param ctx the parse tree
	 */
	void enterBop(BilParser.BopContext ctx);
	/**
	 * Exit a parse tree produced by {@link BilParser#bop}.
	 * @param ctx the parse tree
	 */
	void exitBop(BilParser.BopContext ctx);
	/**
	 * Enter a parse tree produced by {@link BilParser#uop}.
	 * @param ctx the parse tree
	 */
	void enterUop(BilParser.UopContext ctx);
	/**
	 * Exit a parse tree produced by {@link BilParser#uop}.
	 * @param ctx the parse tree
	 */
	void exitUop(BilParser.UopContext ctx);
	/**
	 * Enter a parse tree produced by {@link BilParser#inout}.
	 * @param ctx the parse tree
	 */
	void enterInout(BilParser.InoutContext ctx);
	/**
	 * Exit a parse tree produced by {@link BilParser#inout}.
	 * @param ctx the parse tree
	 */
	void exitInout(BilParser.InoutContext ctx);
	/**
	 * Enter a parse tree produced by {@link BilParser#returnaddr}.
	 * @param ctx the parse tree
	 */
	void enterReturnaddr(BilParser.ReturnaddrContext ctx);
	/**
	 * Exit a parse tree produced by {@link BilParser#returnaddr}.
	 * @param ctx the parse tree
	 */
	void exitReturnaddr(BilParser.ReturnaddrContext ctx);
	/**
	 * Enter a parse tree produced by {@link BilParser#literal}.
	 * @param ctx the parse tree
	 */
	void enterLiteral(BilParser.LiteralContext ctx);
	/**
	 * Exit a parse tree produced by {@link BilParser#literal}.
	 * @param ctx the parse tree
	 */
	void exitLiteral(BilParser.LiteralContext ctx);
	/**
	 * Enter a parse tree produced by {@link BilParser#nat}.
	 * @param ctx the parse tree
	 */
	void enterNat(BilParser.NatContext ctx);
	/**
	 * Exit a parse tree produced by {@link BilParser#nat}.
	 * @param ctx the parse tree
	 */
	void exitNat(BilParser.NatContext ctx);
	/**
	 * Enter a parse tree produced by {@link BilParser#addr}.
	 * @param ctx the parse tree
	 */
	void enterAddr(BilParser.AddrContext ctx);
	/**
	 * Exit a parse tree produced by {@link BilParser#addr}.
	 * @param ctx the parse tree
	 */
	void exitAddr(BilParser.AddrContext ctx);
}