package vcgen

<<<<<<< HEAD
import astnodes.exp.`var`.{MemLoad, Register}
import astnodes.exp.{Expr, Literal}
=======
import astnodes.exp.{Expr, Literal, MemLoad, Var}
>>>>>>> 157a6a8eaa3d618e175e798e48b4b3cd70632d65
import astnodes.stmt.assign.{Assign, GammaUpdate, MemAssign, RegisterAssign}
import astnodes.stmt.{Assert, CJmpStmt, Stmt}
import translating.FlowGraph

import collection.JavaConverters.*
import astnodes.pred.{BinOp, BinOperator, Bool, Pred, conjunct}

object VCGen {
  // This generates the VCs but also updates to gamma variables
  def genVCs(state: State): State = {
    state.copy(functions = state.functions.map(f =>
      f.copy(labelToBlock = f.labelToBlock.map {
<<<<<<< HEAD
        case (pc, b) => (pc, b.copy(lines = b.lines.flatMap(line => List(Assert("TODO", genVC(line, f, state)), line) ++ genGammaUpdate(line, state))))
=======
        case (pc, b) => (pc, b.copy(lines = b.lines.flatMap(line => List(line, Assert("TODO", genVC(line, f, state))) ++ genGammaUpdate(line))))
>>>>>>> 157a6a8eaa3d618e175e798e48b4b3cd70632d65
      })
    ))
  }

  /** Generate the verification condition for a given statement
    */
  def genVC(stmt: Stmt, fState: FunctionState, state: State): Pred = stmt match {
    case cjmpStmt: CJmpStmt     => computeGamma(cjmpStmt.getCondition, state)
    case assign: RegisterAssign => Bool.True // will need to add rely/guar later
    // TODO for each part
    case assign: MemAssign =>
<<<<<<< HEAD
      assign.lhs.onStack match {
        case true  => Bool.True // these are thread local
        case false =>
          // TODO need to be careful bc these could be global or thead local (if they are in the GOT)
          BinOp(BinOperator.Implication, assign.lhs.toL, computeGamma(assign.rhs, state))
=======
      assign.memExp.onStack match {
        case true  => Bool.True // these are thread local
        case false =>
          // TODO need to be careful bc these could be global or thead local (if they are in the GOT)
          BinOp(BinOperator.Implication, assign.memExp.toL, computeGamma(assign.rhsExp))
>>>>>>> 157a6a8eaa3d618e175e798e48b4b3cd70632d65
        // Use the GOT/ST to get the exact variable being referenced, except if it uses pointer arithmetic etc etc
        // would be interesting to see if making the substitution actually made a meaningful difference
      }
    case _: Assert => ??? // There should not be an assert here
    case _         => Bool.True // TODO
  }

<<<<<<< HEAD
  /** Compute the gamma value for an expression
   */
  def computeGamma(expr: Expr, state: State) = expr.vars.map{
    case v: Register => v.toGamma
    case l: MemLoad => BinOp(BinOperator.Disjunction, l.toGamma, l.toL)
  }.conjunct

  /** Generate an assignment to a gamma variable for each variable update.
   */
  def genGammaUpdate(stmt: Stmt, state: State): Option[Stmt] = stmt match {
    case assign: Assign => Some(GammaUpdate(assign.lhs.toGamma, computeGamma(assign.rhs, state)))
    // case assign: RegisterAssign => Some(GammaUpdate(assign.lhs.toGamma, computeGamma(assign.rhs, state)))
=======
  // def computeGamma(expr: Expr) = expr.vars.map(v => v.toGamma).asInstanceOf[List[Pred]].conjunct
  def computeGamma(expr: Expr) = expr.vars.map{
    case v: Var => v.toGamma
    case l: MemLoad => l.toGamma
  }.conjunct
    // Bool.True // expr.vars.map(v => BinOp("&&", v.toGamma, state.getL(v))).conjunct

  def genGammaUpdate(stmt: Stmt): Option[Stmt] = stmt match {
    case assign: MemAssign => Some(GammaUpdate(assign.memExp.toGamma, computeGamma(assign.rhs)))
    case assign: RegisterAssign => Some(GammaUpdate(assign.lhsExp.toGamma, computeGamma(assign.rhs)))
>>>>>>> 157a6a8eaa3d618e175e798e48b4b3cd70632d65
    case _ => None
  }
}

