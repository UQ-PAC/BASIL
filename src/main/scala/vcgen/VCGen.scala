package vcgen

import astnodes.exp.`var`.{MemLoad, Register}
import astnodes.exp.{Expr, Literal}
import astnodes.stmt.assign.{Assign, GammaUpdate, MemAssign, RegisterAssign}
import astnodes.stmt.{Assert, CJmpStmt, Stmt, MethodCall}
import translating.FlowGraph
import astnodes.sec.{SecBinOp, join, Sec}

import collection.JavaConverters.*
import astnodes.pred.{BinOp, BinOperator, Bool, Pred, conjunct, SecComp}

object VCGen {
  // This generates the VCs but also updates to gamma variables
  def genVCs(state: State): State = {
    println("generating VCs")
    state.copy(functions = state.functions.map(f =>
      f.copy(labelToBlock = f.labelToBlock.map {
        case (pc, b) => (pc, b.copy(lines = b.lines.flatMap(line => List(rely, Assert("TODO", genVC(line, f, state)), line) ++ genGammaUpdate(line, state))))
      })
    ))
  }

  /** Generate the verification condition for a given statement
    */
  def genVC(stmt: Stmt, fState: FunctionState, state: State): Pred = stmt match {
    case cjmpStmt: CJmpStmt     => SecComp(computeGamma(cjmpStmt.getCondition, state), state.lattice.attackerLevel)
    case assign: RegisterAssign => Bool.True // will need to add rely/guar later
    // TODO for each part
    case assign: MemAssign =>
      assign.lhs.onStack match {
        case true  => Bool.True // these are thread local
        case false =>
          // TODO need to be careful bc these could be global or thead local (if they are in the GOT)
          SecComp(computeGamma(assign.rhs, state), assign.lhs.toL)
        // Use the GOT/ST to get the exact variable being referenced, except if it uses pointer arithmetic etc etc
        // would be interesting to see if making the substitution actually made a meaningful difference
      }
    case _: Assert => ??? // There should not be an assert here
    case _         => Bool.True // TODO
  }

  /** Compute the gamma value for an expression
   */
  def computeGamma(expr: Expr, state: State): Sec = expr.vars.map{
    case v: Register => v.toGamma
    case l: MemLoad => SecBinOp("meet", l.toGamma, l.toL)
  }.join(state)

  /** Generate an assignment to a gamma variable for each variable update.
   */
  def genGammaUpdate(stmt: Stmt, state: State): Option[Stmt] = stmt match {
    case assign: Assign => Some(GammaUpdate(assign.lhs.toGamma, computeGamma(assign.rhs, state)))
    // case assign: RegisterAssign => Some(GammaUpdate(assign.lhs.toGamma, computeGamma(assign.rhs, state)))
    case _ => None
  }

  def rely = MethodCall("-1", "rely")
}

