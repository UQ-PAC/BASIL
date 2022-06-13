package astnodes

/** Generic instruction fact
  */
trait Stmt(val pc: String, val labelVisible: Boolean = false) {
  def subst(v: Variable, w: Variable): Stmt

  def toBoogieString: String = toString

  def labelString: String = if (labelVisible) {
    "label" + pc + ": "
  } else {
    ""
  }

  //def withVisibleLabel: Stmt
}

case class Assert(override val pc: String, pred: Pred) extends Stmt(pc) {
  override def subst(v: Variable, w: Variable): Stmt = ???

  // TODO is there a way to merge these two methods
  override def toString: String = s"assert $pred;"
  override def toBoogieString: String = s"assert ${pred.toBoogieString};"
}
case class CallStmt(override val pc: String, funcName: String, returnTarget: Option[String], args: List[Register], lhs: Option[Register]) extends Stmt(pc) {

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

  override def subst(v: Variable, w: Variable): Stmt = copy(args = args.map(r => r.subst(v,w) match {
    case x: Register => x
    case _ => ???
  }))


  def libraryFunction: Boolean = CallStmt.libraryFunctions.contains(funcName)
}

case object CallStmt {
  val libraryFunctions: Map[String, LibraryFunction] = List(
    "malloc" -> LibraryFunction(Some(Register("R0", 64)), List(Register("R0", 64))),
    "realloc" -> LibraryFunction(Some(Register("R0", 64)), List()),
    "free" -> LibraryFunction(None, List(Register("R0", 64)), Some("free_"))
  ).toMap

  val callRegisters: IndexedSeq[String] = Range(0, 7).map(x => s"R$x")
}

case class LibraryFunction(lhs: Option[Register], args: List[Register], mappedName: Option[String] = None)

/** Conditional Jump
  */
case class CJmpStmt(override val pc: String,
                    trueTarget: String,
                    falseTarget: String,
                    var condition: Expr,
                   ) extends Stmt(pc) {
  def getCondition: Expr = condition
  override def toString = s"if ($condition) goto label$trueTarget else goto label$falseTarget;"
  override def toBoogieString: String = s"if (bv1tobool(${condition.toBoogieString})) { goto label$trueTarget; } goto label$falseTarget;"

  override def subst(v: Variable, w: Variable): Stmt = this.copy(condition = condition.subst(v,w))
  def fold(old: Expr, sub: Expr): Stmt = this.copy(condition = condition.fold(old, sub))
}

/** Jump
  */
case class JmpStmt(override val pc: String, target: String) extends Stmt(pc) {
  override def toString: String = String.format("%sgoto label%s;", labelString, target)
  override def subst(v: Variable, w: Variable): Stmt = this
}

case class MethodCall(override val pc: String, name: String) extends Stmt(pc) {
  override def subst(v: Variable, w: Variable): Stmt = ???
  override def toString = s"call $name();"
}
/** No instruction fact
  */
case class SkipStmt(override val pc: String) extends Stmt(pc) {
  override def toString: String = String.format("%sskip;", labelString)
  override def subst(v: Variable, w: Variable): Stmt = this

  //override def withVisibleLabel: Stmt = copy(labelVisible = true)
}

// TODO not happy with setup for STMT -> Assign -> MemAssign/RegisterAssign
/** Assignment (e.g. x := facts.exp)
  */
trait Assign(override val pc: String, val lhs: Variable, val rhs: Expr) extends Stmt with MaybeNonConstantAssign {
  override def toString: String = String.format("%s%s := %s;", labelString, lhs, rhs)

  override def subst(v: Variable, w: Variable): Stmt = lhs.subst(v,w) match {
    case lhsRes: MemLoad => MemAssign(pc, lhsRes, rhs.subst(v,w))
    case lhsRes: Register => RegisterAssign(pc, lhsRes, rhs = rhs.subst(v,w))
  }

  // this would be nicer to have per type instead
  def fold(oldExpr: Expr, newExpr: Expr): Assign = {
    lhs match {
      case lhsRes: MemLoad =>
        val newLhs = if (!lhsRes.onStack) {
          lhsRes.fold(oldExpr, newExpr).asInstanceOf[MemLoad]
        } else {
          lhsRes
        }
        MemAssign(pc, newLhs, rhs.fold(oldExpr, newExpr))
      case lhsRes: Register => RegisterAssign(pc, lhsRes, rhs.fold(oldExpr, newExpr))
      case _ => this
    }
  }
}

trait MaybeNonConstantAssign
class NonConstantAssign extends MaybeNonConstantAssign

case class GammaUpdate (lhs: SecVar | SecMemLoad, sec: Sec) extends Stmt("NA") {

  override def toBoogieString: String = s"${lhs.toString} := ${sec.toString};"

  // TODO
  override def subst(v: Variable, w: Variable): Stmt = this
}

/** Memory store
  */
case class MemAssign(override val pc: String, override val lhs: MemLoad, override val rhs: Expr) extends Assign(pc, lhs, rhs) with Stmt(pc) with MaybeNonConstantMemAssign {

  // TODO this tostring method is bad as well
  // need to really sort out a good way to handle the differnet ways memload is presented
  // TODO maybe lhs should not be a memload
  def lhsToString(exp: Expr) = s"heap[${exp.toBoogieString}]"

  override def toBoogieString: String =
    (0 until lhs.size.get / 8).map(n => {
      val newMemExp = BinOp(BinOperator.Addition, lhs.exp, Literal(n.toString))
      s"${lhsToString(newMemExp)} := ${Extract(8 * (n + 1) - 1, 8 * n, rhs).toBoogieString}"
    }).mkString("; ") + s";     // $pc"

}

trait MaybeNonConstantMemAssign
class NonConstantMemAssign extends MaybeNonConstantMemAssign

// TODO check if we need override pc
case class RegisterAssign(override val pc: String, override val lhs: Register, override val rhs: Expr) extends Assign(pc, lhs, rhs) with Stmt(pc) {
  override def toBoogieString: String = s"$labelString${lhs.toBoogieString} := ${rhs.toBoogieString};    // $pc"

  // Otherwise is flag (e.g. #1)
  def isRegister: Boolean = lhs.name.charAt(0) == 'R'

  def isStackPointer: Boolean = this.isRegister && lhs.name.substring(1).equals("31")
  def isFramePointer: Boolean = this.isRegister && lhs.name.substring(1).equals("29")
  def isLinkRegister: Boolean = this.isRegister && lhs.name.substring(1).equals("30")
}

