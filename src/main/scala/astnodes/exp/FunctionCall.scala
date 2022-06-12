package astnodes.exp

import astnodes.exp.variable.Variable
/** Function call
  */
case class FunctionCall(funcName: String, args: List[Expr], override val size: Option[Int] = None) extends Expr {

  /** Value of literal */
  override def toBoogieString = s"$funcName(${args.map(a => a.toBoogieString).mkString(", ")})"

  override def vars: List[Variable] = args.flatMap(a => a.vars)
  override def subst(v: Variable, w: Variable): Expr = copy(args = args.map(a => a.subst(v, w)))
  override def fold(old: Expr, sub: Expr): Expr = this
}
