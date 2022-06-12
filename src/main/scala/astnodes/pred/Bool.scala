package astnodes.pred

import astnodes.exp
import astnodes.exp.variable.{Register, Variable}

import java.util.Collections


case class Bool(name: String) extends Pred {
  override def toString: String = name

  override def vars: List[Register] = List()
  override def substExpr(v: Variable, w: Variable): Pred = this
}

case object Bool {
  val True: Bool = Bool("true")
  val False: Bool = Bool("false")
}

