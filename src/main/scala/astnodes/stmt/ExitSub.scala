package astnodes.stmt

import astnodes.exp.{Expr, Extract}
import astnodes.exp.variable.Variable
import astnodes.parameters.OutParameter

// TODO rename
case class ExitSub(override val pc: String, returnParam: Option[OutParameter]) extends Stmt(pc) {
  // TODO move this logic to boogie translator
  override def toString: String = {
    returnParam match {
      // TODO
      case _ if true => CallStmt.callRegisters.map(x => s"${x}_out := $x;").mkString(" ") + " " + CallStmt.callRegisters.map(x => s"Gamma_${x}_out := Gamma_$x;").mkString(" ") + "\n";
      case Some(out: OutParameter) => s"out := ${out.name};\n"
      case None => ""
    }
  } + "return;"

  override def subst(v: Variable, w: Variable): Stmt = this
}
