package astnodes.pred

import astnodes.exp
import astnodes.exp.`var`.{Register, Var}

import java.util.Collections


case class Bool(name: String) extends Pred {
  override def toString: String = name

  override def vars: List[Register] = List()
  override def substExpr(v: Var, w: Var): Pred = this
}

case object Bool {
  val True: Bool = Bool("true")
  val False: Bool = Bool("false")
}

