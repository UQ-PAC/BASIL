package ir.invariant
import ir._

def singleCallBlockEnd(p: Program): Boolean = {
  p.forall {
    case b: Block => {
      val calls = (b.statements.collect { case c: Call =>
        b.statements.lastOption.contains(c)
      })
      (calls.size <= 1) && calls.headOption.getOrElse(true)
    }
    case _ => true
  }
}
