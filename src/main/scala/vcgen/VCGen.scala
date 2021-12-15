package vcgen

import astnodes.exp.{Expr, Literal, MemLoad, Var}
import astnodes.stmt.assign.{Assign, GammaUpdate, MemAssign, RegisterAssign}
import astnodes.stmt.{Assert, CJmpStmt, Stmt}
import translating.FlowGraph

import collection.JavaConverters.*
import astnodes.pred.{BinOp, BinOperator, Bool, Pred, conjunct}

object VCGen {
  // This generates the VCs but also updates to gamma variables
  def genVCs(state: State): State = {
    // TODO there is a better way to do this deep copy (i assume)
    state.copy(functions = state.functions.map(f =>
      f.copy(labelToBlock = f.labelToBlock.map {
        case (pc, b) => (pc, b.copy(lines = b.lines.flatMap(line => List(line, Assert("TODO", genVC(line, f, state))) ++ genGammaUpdate(line))))
      })
    ))
  }

  /** Generate the VC for a given statement
    *
    * @param stmt
    * @return
    */
  def genVC(stmt: Stmt, fState: FunctionState, state: State): Pred = stmt match {
    case cjmpStmt: CJmpStmt     => computeGamma(cjmpStmt.getCondition)
    case assign: RegisterAssign => Bool.True // will need to add rely/guar later
    case assign: MemAssign =>
      assign.memExp.onStack match {
        case true  => Bool.True // these are thread local
        case false =>
          // TODO need to be careful bc these could be global or thead local (if they are in the GOT)
          BinOp(BinOperator.Implication, assign.memExp.toL, computeGamma(assign.rhsExp))
        // Use the GOT/ST to get the exact variable being referenced, except if it uses pointer arithmetic etc etc
        // would be interesting to see if making the substitution actually made a meaningful difference
      }
    case _: Assert => ??? // There should not be an assert here
    case _         => Bool.True // TODO
  }

  // def computeGamma(expr: Expr) = expr.vars.map(v => v.toGamma).asInstanceOf[List[Pred]].conjunct
  def computeGamma(expr: Expr) = expr.vars.map{
    case v: Var => v.toGamma
    case l: MemLoad => l.toGamma
  }.conjunct
    // Bool.True // expr.vars.map(v => BinOp("&&", v.toGamma, state.getL(v))).conjunct

  def genGammaUpdate(stmt: Stmt): Option[Stmt] = stmt match {
    case assign: MemAssign => Some(GammaUpdate(assign.memExp.toGamma, computeGamma(assign.rhs)))
    case assign: RegisterAssign => Some(GammaUpdate(assign.lhsExp.toGamma, computeGamma(assign.rhs)))
    case _ => None
  }
}

