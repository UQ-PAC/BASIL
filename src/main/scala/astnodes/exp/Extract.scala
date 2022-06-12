package astnodes.exp

import astnodes.exp.variable.Variable
import analysis.tools.SimplificationUtil

/** Extracts the bits from firstInt to secondInt (inclusive) from variable.
  */
case class Extract(firstInt: Int, secondInt: Int, body: Expr) extends Expr {
  override def toString: String = String.format("%s[%d:%d]", body, firstInt, secondInt)
  override def vars: List[Variable] = body.vars

  override def subst(v: Variable, w: Variable): Expr = this.copy(body = body.subst(v, w))

  override def fold(old: Expr, sub: Expr): Expr =
    SimplificationUtil.bitvecExtract(this.copy(body = body.fold(old, sub)))

  override def size: Option[Int] = Some(
    firstInt - secondInt + 1
  ) // + 1 as extracts are inclusive (e.g. 0:31 has 32 bits)

  override def toBoogieString: String = s"${body.toBoogieString}[${firstInt + 1}:$secondInt]"
}
