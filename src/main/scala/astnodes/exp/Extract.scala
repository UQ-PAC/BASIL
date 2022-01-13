package astnodes.exp

import astnodes.exp.`var`.Var

import java.util.Collections
import java.util
import java.util.Objects

/** Extracts the bits from firstInt to secondInt (inclusive) from variable.
 */ 
case class Extract(firstInt: Int, secondInt: Int, variable: Expr) extends Expr {
  override def toString = String.format("%s[%d:%d]", variable, firstInt, secondInt)
  override def vars = variable.vars

  override def subst(v: Expr, w: Expr): Expr = this.copy(variable = variable.subst(v, w))

  override def size = Some(firstInt - secondInt + 1)  // + 1 as extracts are inclusive (e.g. 0:31 has 32 bits)

  override def toBoogieString: String = s"${variable.toBoogieString}[${firstInt + 1}:$secondInt]"
}
