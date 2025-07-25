package ir.transforms

import ir.*
import ir.cilvisitor.*
import analysis.*

import scala.collection.immutable.SortedMap


/**
 * Transforms program by modifying assignments to local variables and procedure calls to constants if possible, as
 * determined by copy-constant analysis (using the IDE framework). Procedure calls are modified to remove redundant
 * out parameters if they always return a constant value. This could be extended to integrate removal of empty blocks
 * and dead input variables.
 */
class ConstCopyPropTransform(p: Program) extends CILVisitor{
  val results: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]] = InterCopyConst(p, true).analyze()
  private var removedFormalOutParams: Set[LocalVar] = Set()

  override def vstmt(e: Statement): VisitAction[List[Statement]] = {


    e match {
      case l: LocalAssign  =>
        if results.contains(e.successor) then {
          val absState: FlatElement[BitVecLiteral] = results(e.successor)(l.lhs)
            
            l.rhs match {
            case LocalVar(_, _, _) | Register(_, _) if absState != Top & absState != Bottom =>
              ChangeTo(List(LocalAssign(l.lhs, get_bv(absState).get))) //replace rhs with constant

            case _ => SkipChildren()
          }
        }
        else SkipChildren()


      case d: DirectCall if d.outParams.nonEmpty =>

        val vars: List[LocalVar] = d.outParams.keys.toList
        val changed: List[Statement] = vars.foldLeft(List[Statement]()) {
          case (l, formalOutParam) =>
            val actualOutParam = d.outParams.getOrElse(formalOutParam, LocalVar("placeholder", BitVecType(64)))
            val absState: FlatElement[BitVecLiteral] = results(d.successor)(actualOutParam)



            if results(d.target.returnBlock.get.jump)(formalOutParam) != Top then //outParam from procedure always constant

              d.outParams = d.outParams.removed(formalOutParam)
              d.target.formalOutParam.remove(formalOutParam) //remove from called procedure
              removedFormalOutParams = removedFormalOutParams + formalOutParam
              l ++ List(LocalAssign(actualOutParam, get_bv(absState).get)) // add assignment

            else if absState != Top & absState != Bottom then //outParam from procedure constant for this call

              d.outParams = d.outParams.removed(formalOutParam) //remove assignment of x = f(y) --> f(y) alone
              l ++ List(LocalAssign(actualOutParam, get_bv(absState).get)) // add assignment without changing function

            else l

        }

        val transformed = changed ++ List(d)

        ChangeTo(transformed)

      case _ => SkipChildren()
    }
  }

  override def vjump(j: Jump): VisitAction[Jump] =  {
    j match {
      case r: Return =>
        r.outParams = r.outParams.foldLeft(SortedMap[LocalVar, Expr]()) {
          case (m, (l, e)) =>
          if removedFormalOutParams.contains(l) then m else m ++ Map(l->e)
          // remove return params which are no longer needed
        }
      case _ =>

    }
    SkipChildren()
  }
}


/**
 * Extract actual BitVecLiteral from given FlatElement of lattice. Do not use unless it is known that the FlatElement
 * contains a BitVecLiteral and not Top/Bottom
 */
def get_bv(a: FlatElement[BitVecLiteral]): Option[BitVecLiteral] =
  a match
    case FlatEl(x) => Some(x)
    case _ => None // SHOULD BE UNREACHABLE



