package util
// import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.TerminalNode;

import BilParser.BilAdtBaseListener;

class TalkingListener extends BilAdtBaseListener {

    // @Override public void enterEveryRule(ParserRuleContext ctx) { 

	// 	System.out.println("enterRule:\n" + ctx.getText());
	// }
	
	// @Override public void exitEveryRule(ParserRuleContext ctx) { 
	// 	System.out.println("exitRule:\n" + ctx.getText());
	// }
	
	override def visitTerminal(node: TerminalNode): Unit =  { 
		println("enterTerminal:\n" + node.getText());
	}
}