package astnodes.stmt

import util.AssumptionViolationException

import java.util.{Collections, List}
import astnodes.exp.Expr
import astnodes.exp.`var`.{Register, Var}

// TODO is this class necassary 
<<<<<<< HEAD
case class InitStmt(variable: Register, label: String, val varType: String, val const: Boolean = false) extends Stmt(label) {
  override def subst(v: Var, w: Var): Stmt = variable.subst(v,w) match {
    case res: Register => this.copy(variable = res)
    case _ => ???
  }
=======
class InitStmt(var variable: Var, label: String, val varType: String, val const: Boolean = false) extends Stmt(label) {
  override def getChildren: List[Expr]  =  Collections.singletonList(variable)
  override def replace(oldExp: Expr, newExp: Expr)  =  if (variable == oldExp)  variable = newExp.asInstanceOf[Var]
>>>>>>> 157a6a8eaa3d618e175e798e48b4b3cd70632d65
  override def toString  =  String.format("%s %s: %s;", if (const) "const" else "var", variable, varType)

  // TODO not 100% happy with this
  // TODO removing this class and instead getting all of the variables (including gamma vars) would be neater
  override def toBoogieString: String = f"${if (const) "const" else "var"} $variable: $varType; " +
    f"${if (const) "const" else "var"} Gamma_$variable: ${
      if (varType.startsWith("bv")) "bool"
      else if (varType == "[bv64] bv8") "[bv64] bool"
      else if (varType == "[bv64] bool") "[bv64] bool" // TODO remove
      else throw new AssumptionViolationException(s"Unhandled type $varType")
    };"

}