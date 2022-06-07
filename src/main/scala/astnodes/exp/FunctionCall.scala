package astnodes.exp

import astnodes.exp.`var`.{Register, Var}

import java.util
import java.util.Objects

/** Function call
  */
case class FunctionCall(funcName: String, args: List[Expr], override val size: Option[Int] = None) extends Expr {
  /** Value of literal */
  override def toBoogieString = s"$funcName(${args.map(a => a.toBoogieString).mkString(", ")})"

  override def vars: List[Var] = args.flatMap(a => a.vars)
  override def subst(v: Var, w: Var): Expr = copy(args = args.map(a => a.subst(v,w)))
  override def fold(old: Expr, sub: Expr): Expr = this
}

