package astnodes.stmt

import astnodes.parameters.InParameter
import astnodes.exp.Expr
import astnodes.exp.`var`.Var
import astnodes.parameters.OutParameter

import scala.collection.immutable
import astnodes.pred.Pred
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Buffer
import astnodes.Label

// TODO scalaify
// TODO rewrite statment loader to remove getters
// TODO remove the need for this, and instead create directly place this logic in function state
//      as we know the BIL output I think this would be fine
case class EnterSub(val pc: String, funcName: String, var requires: List[Pred], var ensures: List[Pred]) extends Stmt(Label(pc)) {
  private var inParams: Buffer[InParameter] = new ArrayBuffer[InParameter]()
  private var outParam: Option[OutParameter] = None
  private var modifies: Buffer[String] = new ArrayBuffer[String]() // TODO type
  // TODO dont like this
  modifies.addAll(immutable.List("heap", "stack", "Gamma_heap", "Gamma_stack", "SP", "R31", "Gamma_SP", "Gamma_R31"))

  def getInParams = inParams
  def getOutParam = outParam
  def setOutParam(outParam: OutParameter) = this.outParam = Some(outParam)
  def setInParams(newInParms: Buffer[InParameter]) = inParams = newInParms

  def setRequiresEnsures(requires: List[Pred], ensures: List[Pred]) = {
    this.requires = requires
    this.ensures = ensures
  }

  override def toString = {
    val in = 
      if (libraryFunction) inParams.mkString(", ")
      else CallStmt.callRegisters.map(x => s"${x}_in: bv64").mkString(", ") + ", " + CallStmt.callRegisters.map(x => s"Gamma_${x}_in: SecurityLevel").mkString(", ")
    val out = 
      // TODO gamma out
      if (!libraryFunction) "returns (" + CallStmt.callRegisters.map(x => s"${x}_out: bv64").mkString(", ") + ", " + CallStmt.callRegisters.map(x => s"Gamma_${x}_out: SecurityLevel").mkString(", ") + ")"
      else if (outParam != None) f" returns (${outParam.get})" else ""

    val decl = funcName + "(" + in + ")" + out

    // TODO
    val modifiesStr =
      if (modifies.size > 0) "\n modifies " + modifies.mkString(", ") //+ ";\nimplementation " + decl
      else ""

    val requiresStr = requires.map(r => s"requires $r;").mkString(" ")
    val ensuresStr = ensures.map(e => s"ensures $e;").mkString(" ")

    s"procedure $decl $modifiesStr; $requiresStr $ensuresStr {"
  }

  override def subst(v: Var, w: Var): Stmt = this
  def libraryFunction = CallStmt.libraryFunctions.contains(funcName)
}
