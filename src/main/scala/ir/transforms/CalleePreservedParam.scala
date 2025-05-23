package ir
package transforms

import cilvisitor.*

object CalleePreservedParam {

  /**
   * Asusming single-return and parameter form 
   */

  val preserved = (19 to 29).map(i => s"R$i").toSet

  def isPreservedParam(v: LocalVar) = {
    // abi assumption
    v.varName match {
      case "R29_in" | "R30_in" | "R31_in" | "R29" | "R30" | "R31" | "R29_out" | "R30_out" | "R31_out" => true
      case o if preserved.contains(o.stripSuffix("_in").stripSuffix("_out")) => true
      case _ => false
    }
  }

  def transform(p: Program) = {
    val v = MakePreserved()
    visit_prog(v, p)
    assert(invariant.correctCalls(p))
  }

  class MakePreserved extends CILVisitor {

    override def vproc(p: Procedure) = {

      for (param <- p.formalOutParam.filter(isPreservedParam)) {

        val input = p.formalInParam.find(_.name.startsWith(param.varName.stripSuffix("_out"))).get

        p.formalOutParam.remove(param)

        for (call <- p.incomingCalls()) {
          call.outParams = call.outParams.removed(param)
        }

        p.returnBlock.foreach(b => {
          val out = b.jump match {
            case r: Return => {
              val arg = r.outParams(param)
              r.outParams = r.outParams.removed(param)
              arg
            }
            case o => ???
          }
          b.statements.append(
            Assert(BinaryExpr(EQ, input, out), Some(s"${param.name.stripSuffix("_out")} preserved across calls"))
          )
        })
      }
      SkipChildren()
    }

  }

}
