package astnodes

import scala.collection.mutable.ArrayBuffer

// TODO rewrite statment loader to remove getters
// TODO remove the need for this, and instead create directly place this logic in function state
//      as we know the BIL output I think this would be fine
case class EnterSub(
    override val pc: String,
    funcName: String,
    var requires: List[Pred],
    var ensures: List[Pred],
    inParams: ArrayBuffer[InParameter] = ArrayBuffer(),
    var outParam: Option[OutParameter] = None,
    modifies: ArrayBuffer[String] =
      ArrayBuffer("heap", "stack", "Gamma_heap", "Gamma_stack", "SP", "R31", "Gamma_SP", "Gamma_R31")
) extends Stmt(pc) {
  override def toString: String = {
    val in =
      if (libraryFunction) inParams.mkString(", ")
      else
        CallStmt.callRegisters.map(x => s"${x}_in: bv64").mkString(", ") + ", " + CallStmt.callRegisters
          .map(x => s"Gamma_${x}_in: SecurityLevel")
          .mkString(", ")
    val out =
      // TODO gamma out
      if (!libraryFunction)
        "returns (" + CallStmt.callRegisters.map(x => s"${x}_out: bv64").mkString(", ") + ", " + CallStmt.callRegisters
          .map(x => s"Gamma_${x}_out: SecurityLevel")
          .mkString(", ") + ")"
      else if (outParam.isDefined) f" returns (${outParam.get})"
      else ""

    val decl = funcName + "(" + in + ")" + out

    // TODO
    val modifiesStr =
      if (modifies.nonEmpty) "\n modifies " + modifies.mkString(", ") //+ ";\nimplementation " + decl
      else ""

    val requiresStr = requires.map(r => s"requires $r;").mkString(" ")
    val ensuresStr = ensures.map(e => s"ensures $e;").mkString(" ")

    s"procedure $decl $modifiesStr; $requiresStr $ensuresStr {"
  }

  override def subst(v: Variable, w: Variable): Stmt = this
  def libraryFunction: Boolean = CallStmt.libraryFunctions.contains(funcName)
}
