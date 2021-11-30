package facts.pred

import java.util.Collections

case class Bool(name: String) extends Pred {
  override def toString: String = name
}

case object Bool {
  val True = Bool("True")
  val False = Bool("False")
}

