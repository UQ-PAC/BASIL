package ir.transforms

import ir.*
import ir.cilvisitor.*
import analysis.*

class ConstCopyPropTransform(p: Program) extends CILVisitor{
  val results: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]] = InterCopyConst(p, true).analyze()

  override def vstmt(e: Statement): VisitAction[List[Statement]] = {


    e match {
      case l: LocalAssign  =>
        val absState: FlatElement[BitVecLiteral] = results.get(e.successor).get(l.lhs)

        l.rhs match {
          case LocalVar(_,_,_) | Register(_,_) if absState != Top | absState != Bottom =>
            ChangeTo(List(LocalAssign(l.lhs, get_bv(absState))))

          case _ => SkipChildren()
        }

      case d: DirectCall if d.outParams.nonEmpty => // change so chekc if there are any acc outparams
        print(d)

        val vars: List[Variable] = d.outParams.values.toList
        val changed: List[Statement] = vars.foldLeft(List[Statement]()) {
          case (l, lhs) =>
            val absState: FlatElement[BitVecLiteral] = results.get(e.successor).get(lhs)
            if absState != Top | absState != Bottom then l ++ List(LocalAssign(lhs, get_bv(absState)))
            else l 
            // need t ohandle when some top some not!! <-- unchanged and changed list!!
        }
        
        //val untransformedParams = d.outParams.values.toList.foldLeft(List[])

        //val transformed = changed ++ List(d)

        ChangeTo(changed) // need better name than cchangfed LMAO

      case _ => SkipChildren()
    }
  }
}



def get_bv(a: FlatElement[BitVecLiteral]): BitVecLiteral =
  a match
    case FlatEl(x) => x
    case _ => BitVecLiteral(0,0) // shldnt get here, idk what default to iuse



