package translating

import BilParser.{SymsBaseListener, SymsParser}
import astnodes.exp.{Literal, Var}

import scala.collection.mutable

class SymbolTableListener extends SymsBaseListener {
  val symbolTable = new mutable.HashMap[Literal, Var]()

  override def exitSymbolTableRow(ctx: SymsParser.SymbolTableRowContext): Unit =
    // if (ctx.bind.getText == "GLOBAL") symbolTable.put(new Literal(ctx.HEX(1).getText), Var(ctx.name.getText))
    if (ctx.ALPHA(1).getText == "GLOBAL") symbolTable.put(new Literal("0x" + ctx.HEX(1).getText), Var(ctx.name.getText))
}
