package ir.invariant
import ir._


/*
 * DirectCalls are provided actual and return parameters matchign their target's parameters
 */


def correctCalls(p: Program) : Boolean = {
  p.forall {
    case c: DirectCall => {
      val t = c.target
      (c.actualParams.keySet == t.formalInParam) && (c.outParams.keySet == t.formalOutParam) 
      && c.actualParams.forall((k, v) => k.getType == v.getType) && c.outParams.forall((k,v) => k.getType == v.getType)
    }
    case _ => true
  }
}


