package util
// import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.TerminalNode;

import BilParser.BilAdtBaseListener;
import BilParser.BilAdtParser.UuidContext
import com.google.protobuf.ByteString
import java.util.Base64

class TalkingListener extends BilAdtBaseListener {

    // @Override public void enterEveryRule(ParserRuleContext ctx) { 

	// 	System.out.println("enterRule:\n" + ctx.getText());
	// }
	
	// @Override public void exitEveryRule(ParserRuleContext ctx) { 
	// 	System.out.println("exitRule:\n" + ctx.getText());
	// }
	
	// override def visitTerminal(node: TerminalNode): Unit =  { 
	// 	println("enterTerminal:\n" + node.getText());
	// }

	override def enterUuid(ctx: UuidContext): Unit = {
		val byte = ByteString.copyFrom(Base64.getDecoder().decode(ctx.getText()))
		println(ctx.getText() + " , " + byte)
	}
}