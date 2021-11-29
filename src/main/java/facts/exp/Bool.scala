package facts.exp

import java.util.Collections

case class Bool(name: String) extends Expr {
  // Bools can only occur in predicates, so these methods should not be called
  override def getChildren = ???
  override def replace(oldExp: Expr, newExp: Expr) = ???
  override def vars: List[Var] = ???

  override def toString: String = name
}

case object Bool {
  val True = Bool("True")
  val False = Bool("False")
}

