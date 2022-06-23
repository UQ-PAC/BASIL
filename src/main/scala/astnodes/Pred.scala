
/*
package astnodes
trait Pred {
  //def vars: List[Variable]
  def substExpr(v: Variable, w: Variable): Pred
  def toBoogieString: String = toString
}

/** Define custom methods for List[Expr]
  */
extension (preds: List[Pred]) {
  def conjunct: Pred = preds match {
    case pred :: Nil => pred
    case pred :: rest => PredBinOp(PredBinOperator.Conjuction, pred, rest.conjunct)
    case Nil => Bool.True
  }
}

case class Bool(name: String) extends Pred {
  override def toString: String = name

 // override def vars: List[LocalVar] = List()
  override def substExpr(v: Variable, w: Variable): Pred = this
}

case object Bool {
  val True: Bool = Bool("true")
  val False: Bool = Bool("false")
}

case class ExprComp(op: String, firstExpr: Expr, secondExpr: Expr) extends Pred {
  //override def vars: List[Variable] = firstExpr.vars ++ secondExpr.vars
  override def toString: String = s"(${firstExpr.toBoogieString} $op ${secondExpr.toBoogieString})"
  override def substExpr(v: Variable, w: Variable): Pred = copy(firstExpr = firstExpr.subst(v, w), secondExpr = secondExpr.subst(v,w))
}

case class PredBinOp(op: PredBinOperator, firstPred: Pred, secondPred: Pred) extends Pred {
  def this(operatorStr: String, firstPred: Pred, secondPred: Pred) = this(PredBinOperator.valueOf(operatorStr), firstPred, secondPred)
  //override def vars: List[Variable] = firstPred.vars ++ secondPred.vars
  override def toString = s"($firstPred $op $secondPred)"
  override def substExpr(v: Variable, w: Variable): Pred = copy(firstPred = firstPred.substExpr(v,w), secondPred = secondPred.substExpr(v,w))
}

enum PredBinOperator(val boogieRepr: String) {
  case Implication extends PredBinOperator("==>")
  case Conjuction extends PredBinOperator("&&")
  case Disjunction extends PredBinOperator("||")
  case Equality extends PredBinOperator("==")
  case InEquality extends PredBinOperator("!=")

  override def toString: String = boogieRepr
}

case class PredUniOp(op: String, pred: Pred) extends Pred {
  //override def vars: List[Variable] = pred.vars
  override def toString: String = String.format("%s (%s)", op, pred)
  override def substExpr(v: Variable, w: Variable): Pred = copy(pred = pred.substExpr(v,w))
}

// TODO fix types
case class Forall(ids: String, pred: String) extends Pred {
  //override def vars: List[Variable] = ???
  override def toString = s"(forall $ids :: $pred)"
  override def substExpr(v: Variable, w: Variable): Pred = ???
}

/** If then else expression
  */
case class ITE(cond: Pred, firstPred: Pred, secondPred: Pred) extends Pred {
  //override def vars: List[Variable] = cond.vars ++ firstPred.vars ++ secondPred.vars
  override def toString = s"if ($cond) then $firstPred else $secondPred"
  override def substExpr(v: Variable, w: Variable): Pred = ???
}

/**
  *
  */
case class SecComp(first: Sec, second: Sec) extends Pred {
  //override def vars: List[Variable] = ???
  override def toString: String = s"($first <: $second)"
  override def substExpr(v: Variable, w: Variable): Pred = ???
}
case class PredMemLoad(loc: Expr) extends Pred {
  //override def vars: List[LocalVar] = ??? // TODO we cant handle this atm
  override def toString = s"${if (this.onStack) "stack" else "heap"}[${loc.toBoogieString}]"

  def onStack: Boolean = loc match {
    // TODO improve
    case v: LocalVar => v.name == "R31"
    case PredBinOp(_, v: LocalVar, _) => v.name == "R31"
    case _ => false
  }


  override def substExpr(v: Variable, w: Variable): Pred = ???
}

// TODO are all predicate variables gamma?
case class PredVariable(name: String) extends Pred {
  //override def vars: List[LocalVar] = ??? // List(new exp.Var(name))

  override def toString: String = name
  override def substExpr(v: Variable, w: Variable): Pred = this
}
*/