package ir.invariant
import ir.*
import util.Logger

/*
 * DirectCalls are provided actual and return parameters matchign their target's parameters
 */

def correctCalls(p: Program): Boolean = {
  p.forall {
    case c: DirectCall => {
      val t = c.target
      val inparams = (c.actualParams.keySet == t.formalInParam)
      val outparams = (c.outParams.keySet == t.formalOutParam)
      val typecheck =
        c.actualParams.forall((k, v) => k.getType == v.getType) && c.outParams.forall((k, v) => k.getType == v.getType)
      val r = inparams && outparams && typecheck
      if (!inparams) {
        Logger.error(
          s"call in arguments don't match formal params: ${c.target.name}(${c.actualParams}) != ${c.target.formalInParam}"
        )
      }
      if (!outparams) {
        Logger.error(
          s"call return lvalue doesnt match out params: ${c.outParams} := ${c.target.name}() = ${c.target.formalOutParam}"
        )
      }
      if (!typecheck) {
        val inp = c.actualParams.collect {
          case (k, v) if k.getType != v.getType => s"$k := $v"
        }
        val out = c.outParams.collect {
          case (k, v) if k.getType != v.getType => s"$v := $k"
        }
        Logger.error(s"Call $c doesn't typecheck${if inp.nonEmpty then " in : " else ""}${inp
            .mkString(" ")}${if out.nonEmpty then " out : " else ""}${out.mkString(" ")}")
      }

      r
    }
    case c: Return => {
      val t = c.parent.parent
      val outparams = (c.outParams.keySet == t.formalOutParam)
      val typecheck =
        c.outParams.forall((k, v) => k.getType == v.getType) && c.outParams.forall((k, v) => k.getType == v.getType)
      if (!outparams) {
        Logger.error(
          s"Return formal out params do  do not match procedure formal out params: return(${c.outParams}) != (${t.name}) -> ${t.formalOutParam}"
        )
      }
      if (!typecheck) {
        val out = c.outParams.collect {
          case (k, v) if k.getType != v.getType => s"$k := $v"
        }
        Logger.error(s"doesn't typecheck:$c  \n${if out.nonEmpty then " out : " else ""}${out.mkString(" ")}")
      }
      typecheck && outparams
    }
    case _ => true
  }
}
