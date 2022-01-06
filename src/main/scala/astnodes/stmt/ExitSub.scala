package astnodes.stmt

import java.util
import astnodes.exp.Expr
import astnodes.exp.`var`.Var
import astnodes.parameters.OutParameter
import astnodes.exp.Extract
import astnodes.Label

// TODO rename
case class ExitSub(val pc: String, returnParam: Option[OutParameter]) extends Stmt(Label(pc)) {
  // TODO move this logic to boogie translator
  override def toString = {returnParam match {
    case Some(out) => s"out := ${out.getName};\n"
    case None => ""
  }} + "return;"

  override def subst(v: Var, w: Var): Stmt = this
}
