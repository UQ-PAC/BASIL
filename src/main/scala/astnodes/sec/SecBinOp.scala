package astnodes.sec
import astnodes.exp.variable.Variable

case class SecBinOp(op: String, first: Sec, second: Sec) extends Sec {
  override def toString = s"$op($first, $second)"
  override def vars: List[Variable] = first.vars ++ second.vars
}

