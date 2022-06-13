package translating

import BilParser.{SymsBaseListener, SymsParser}
import astnodes.*

import scala.collection.mutable

class SymbolTableListener extends SymsBaseListener {
  val symbolTable = new mutable.HashMap[String, Literal]()

  override def exitSymbolTableRow(ctx: SymsParser.SymbolTableRowContext): Unit =
    // TODO can grab other things (e.g. size). I think size = size * 16
    if (ctx.ALPHA(1).getText == "GLOBAL") symbolTable.put(ctx.name.getText, Literal("0x" + ctx.HEX(1).getText, Some(64)))
}
