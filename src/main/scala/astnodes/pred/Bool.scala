package astnodes.pred

import astnodes.exp.Var

import java.util.Collections

case class Bool(name: String) extends Pred {
  override def toString: String = name
  override def vars: List[Var] = List()
}

case object Bool {
  val True = Bool("True")
  val False = Bool("False")
}

