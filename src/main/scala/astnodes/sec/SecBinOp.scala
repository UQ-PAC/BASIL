package astnodes.sec
import astnodes.exp.`var`.Var

case class SecBinOp(op: String, first: Sec, second: Sec) extends Sec {
  override def toString = s"$op($first, $second)"
  override def vars: List[Var] = first.vars ++ second.vars
}

