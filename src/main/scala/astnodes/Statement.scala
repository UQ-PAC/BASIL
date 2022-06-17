package astnodes

trait Statement extends Node {
  //def subst(v: Variable, w: Variable): Stmt

  def toBoogieString: String = toString

  /*
  def labelString: String = if (labelVisible) {
    "label" + pc + ": "
  } else {
    ""
  }
  */

  //def withVisibleLabel: Stmt
}

case class Assert(pred: Pred) extends Statement {
  //override def subst(v: Variable, w: Variable): Stmt = ???

  override def toString: String = toBoogieString
  override def toBoogieString: String = s"assert ${pred.toBoogieString};"
}

case class DirectCall(target: String, condition: Expr, returnTarget: Option[String]) extends Statement {
  override def toBoogieString: String = {
    if (condition == Literal(BigInt(1), 1)) {
      noCondition
    } else {
      "if (" + condition.toBoogieString + " == 0bv1) {\n      " + noCondition + "\n    }"
    }
  }
  def noCondition: String = "call " + target + "(); // with return " + returnTarget.getOrElse("none")
}

case class IndirectCall(target: LocalVar, condition: Expr, returnTarget: Option[String]) extends Statement {
  override def toBoogieString: String = {
    if (condition == Literal(BigInt(1), 1)) {
      noCondition
    } else {
      "if (" + condition.toBoogieString + " == 0bv1) {\n      " + noCondition + "\n    }"
    }
  }
  def noCondition: String = "call " + target.toBoogieString + "; // with return " + returnTarget.getOrElse("none")
}

case class GoTo(target: String, condition: Expr) extends Statement {
  override def toBoogieString: String = {
    if (condition == Literal(BigInt(1), 1)) {
      noCondition
    } else {
      "if (" + condition.toBoogieString + " == 0bv1) {\n      " + noCondition + "\n    }"
    }
  }
  def noCondition: String = "goto " + target + ";"
}



/*
case class CallStmt(funcName: String, returnTarget: Option[String], args: List[LocalVar], lhs: Option[LocalVar]) extends Statement {

  override def toString: String = {
    // TODO neaten up call args logic
    val argsStr =
      if (libraryFunction) args.map(arg => arg.name).mkString(", ")
      else CallStmt.callRegisters.map(x => s"$x: bv64").mkString(", ") + ", " + CallStmt.callRegisters.map(x => s"Gamma_$x: SecurityLevel").mkString(", ")

    val lhsStr = lhs match {
      case _ if !libraryFunction => CallStmt.callRegisters.mkString(", ") + ", " + CallStmt.callRegisters.map(x => s"Gamma_$x").mkString(", ") + " := "
      case Some(x: Variable) => s"$x, ${x.toGamma} := "
      case None => ""
    }
    val targetStr = returnTarget match {
      case Some(x) => s"goto label$x;"
      case None => ""
    }
    val name =
      if (libraryFunction) CallStmt.libraryFunctions(funcName).mappedName.getOrElse(funcName)
      else funcName

    s"call $labelString$lhsStr $name ($argsStr); $targetStr"
  }

  /*
  override def subst(v: Variable, w: Variable): Stmt = copy(args = args.map(r => r.subst(v,w) match {
    case x: Register => x
    case _ => ???
  }))
  */

  def libraryFunction: Boolean = CallStmt.libraryFunctions.contains(funcName)
}

case object CallStmt {
  val libraryFunctions: Map[String, LibraryFunction] = List(
    "malloc" -> LibraryFunction(Some(LocalVar("R0", 64)), List(LocalVar("R0", 64))),
    "realloc" -> LibraryFunction(Some(LocalVar("R0", 64)), List()),
    "free" -> LibraryFunction(None, List(LocalVar("R0", 64)), Some("free_"))
  ).toMap

  val callRegisters: IndexedSeq[String] = Range(0, 7).map(x => s"R$x")
}

case class LibraryFunction(lhs: Option[LocalVar], args: List[LocalVar], mappedName: Option[String] = None)
*/

/** Conditional Jump
  */
/*
case class CJmpStmt(override val pc: String,
                    trueTarget: String,
                    falseTarget: String,
                    condition: Expr,
                   ) extends Statement(pc) {
  def getCondition: Expr = condition
  override def toString = s"if ($condition) goto label$trueTarget else goto label$falseTarget;"
  override def toBoogieString: String = s"if (bv1tobool(${condition.toBoogieString})) { goto label$trueTarget; } goto label$falseTarget;"

  //override def subst(v: Variable, w: Variable): Stmt = copy(condition = condition.subst(v,w))
  def simplify(old: Expr, sub: Expr): Statement = copy(condition = condition.simplify(old, sub))
}
*/

/** Jump
  */
/*
case class JmpStmt(target: String) extends Statement(pc) {
  override def toString: String = String.format("%sgoto label%s;", labelString, target)
  //override def subst(v: Variable, w: Variable): Stmt = this
}
*/

/*
case class MethodCall(name: String) extends Statement(pc) {
  //override def subst(v: Variable, w: Variable): Stmt = ???
  override def toString = s"call $name();"
}
*/


/** No instruction fact
  */
object Skip extends Statement {
  override def toString: String = "skip;"
  //override def subst(v: Variable, w: Variable): Stmt = this

  //override def withVisibleLabel: Stmt = copy(labelVisible = true)
}

// TODO not happy with setup for STMT -> Assign -> MemAssign/RegisterAssign
/** Assignment (e.g. x := facts.exp)
  */


trait AssignStatement(lhs: Variable, rhs: Expr) extends Statement with MaybeNonConstantAssign {
  override def toString: String = String.format("%s := %s;", lhs, rhs)

  /*
  override def subst(v: Variable, w: Variable): Stmt = lhs.subst(v,w) match {
    case lhsRes: MemLoad => MemAssign(pc, lhsRes, rhs.subst(v,w))
    case lhsRes: Register => RegisterAssign(pc, lhsRes, rhs = rhs.subst(v,w))
  }
  */

  // this would be nicer to have per type instead
  /*
  def simplify(oldExpr: Expr, newExpr: Expr): AssignStatement = {
    lhs match {
      case lhsRes: MemAccess =>
        val newLhs = if (!lhsRes.onStack) {
          lhsRes.simplify(oldExpr, newExpr).asInstanceOf[MemAccess]
        } else {
          lhsRes
        }
        MemAssign(newLhs, rhs.simplify(oldExpr, newExpr))
      case lhsRes: LocalVar => LocalAssign(lhsRes, rhs.simplify(oldExpr, newExpr))
      case _ => this
    }
  }
  */
}

trait MaybeNonConstantAssign
class NonConstantAssign extends MaybeNonConstantAssign

case class GammaUpdate (lhs: SecVar | SecMemLoad, sec: Sec) extends Statement {

  override def toBoogieString: String = s"${lhs.toString} := ${sec.toString};"

  // TODO
  //override def subst(v: Variable, w: Variable): Stmt = this
}

/** Memory store
  */
case class MemAssign(lhs: MemAccess, rhs: Expr) extends AssignStatement(lhs, rhs) with MaybeNonConstantMemAssign {

  // TODO this tostring method is bad as well
  // need to really sort out a good way to handle the differnet ways memload is presented
  // TODO maybe lhs should not be a memload

  override def toBoogieString: String = s"${lhs.toBoogieString} := ${rhs.toBoogieString};"
  /*
  def lhsToString(exp: Expr) = s"heap[${exp.toBoogieString}]"

  override def toBoogieString: String = {
    (0 until lhs.size / 8).map(n => {
      s"heap[${lhs.index.toBoogieString} + $n] := ${Extract(8 * (n + 1) - 1, 8 * n, rhs).toBoogieString}"
    }).mkString("; ") + s";"
  }
  */

}

trait MaybeNonConstantMemAssign
class NonConstantMemAssign extends MaybeNonConstantMemAssign

case class LocalAssign(lhs: LocalVar, rhs: Expr) extends AssignStatement(lhs, rhs) {
  override def toBoogieString: String = s"${lhs.toBoogieString} := ${rhs.toBoogieString};"

  // Otherwise is flag (e.g. #1)
  def isRegister: Boolean = lhs.name.charAt(0) == 'R'

  def isStackPointer: Boolean = this.isRegister && lhs.name.substring(1).equals("31")
  def isFramePointer: Boolean = this.isRegister && lhs.name.substring(1).equals("29")
  def isLinkRegister: Boolean = this.isRegister && lhs.name.substring(1).equals("30")
}

