package ir
package transforms
import util.assertion.*

import cilvisitor.*

object CalleePreservedParam {

  var counter = util.Counter()

  /**
   * Asusming single-return and parameter form 
   */

  val preserved = (19 to 29).map(i => s"R$i").toSet // required by ABI
  val callReturnStackPreserved = Set("R30", "R31") // not strictly required

  def isPreservedParam(v: LocalVar) = {
    // abi assumption
    v.varName match {
      case s"${reg}_in" => callReturnStackPreserved.contains(reg)
      case s"${reg}_out" => callReturnStackPreserved.contains(reg)
      case s"${reg}_in" => preserved.contains(reg)
      case s"${reg}_out" => preserved.contains(reg)
      case _ => false
    }
  }

  // returns labels of injected assertions
  def transform(p: Program): List[String] = {
    val v = MakePreserved()
    visit_prog(v, p)
    debugAssert(invariant.correctCalls(p))
    v.addedAsserts
  }

  class MakePreserved extends CILVisitor {

    var addedAsserts = List[String]()

    override def vproc(p: Procedure) = {

      for (param <- p.formalOutParam.filter(isPreservedParam)) {

        val input = p.formalInParam.collect {
          case p @ LocalVar(s"${reg}_in", _, _) if reg == param.name.stripSuffix("_out") => p
        }.head
        // we don't expect it to be possible for these params to be removed from the param list
        // since they are preserved across calls they must be live at entry

        p.formalOutParam.remove(param)

        for (call <- p.incomingCalls()) {
          call.outParams = call.outParams - param
        }

        p.returnBlock.foreach(b => {
          val out = b.jump match {
            case r: Return => {
              val arg = r.outParams(param)
              r.outParams = r.outParams - param
              arg
            }
            case o => ???
          }
          var l = Some(s"callerpreserved${counter.next()}")
          val assert =
            Assert(BinaryExpr(EQ, input, out), Some(s"${param.name.stripSuffix("_out")} preserved across calls"), l)
          b.statements.append(assert)
          addedAsserts = l.get :: addedAsserts
        })
      }
      SkipChildren()
    }

  }

}
