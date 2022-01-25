package astnodes.sec

case class SecBinOp(op: String, first: Sec, second: Sec) extends Sec {
  override def toString = s"$op($first, $second)"
  override def vars = first.vars ++ second.vars
}
