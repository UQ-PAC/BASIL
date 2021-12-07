package vcgen

import astnodes.exp.{Expr, Literal, Var}
import astnodes.stmt.assign.{Assign, MemAssign, RegisterAssign}
import astnodes.stmt.{Assert, CJmpStmt, Stmt}
import translating.FlowGraph

import collection.JavaConverters.*
import astnodes.pred.{Bool, Pred, BinOp, conjunct}

object VCGen {
  def genVCs(state: State): State = {
    // TODO there is a better way to do this deep copy (i assume)
    state.copy(functions = state.functions.map(f =>
      f.copy(labelToBlock = f.labelToBlock.map {
        case (pc, b) => (pc, b.copy(lines = b.lines.flatMap(line => List(Assert("TODO", genVC(line, f, state)), line))))
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
        case true  => Bool.True // TODO aren't these thread local
        case false => BinOp("=>", assign.memExp.toL, computeGamma(assign.rhsExp))
        // Use the GOT/ST to get the exact variable being referenced, except if it uses pointer arithmetic etc etc
        // would be interesting to see if making the substitution actually made a meaningful difference
      }
    // TODO do we treat this like an array / map (or we could convert to vars but that seems like a bad idea)
    // TODO ^ my current thinking is that we specify using the original vars, use the GOT/symbol table to generate a corresponding specification
    // on the memory
    case _: Assert => ??? // There should not be an assert here
    case _         => Bool.True // TODO
  }

  // def computeGamma(expr: Expr) = expr.vars.map(v => v.toGamma).asInstanceOf[List[Pred]].conjunct
  def computeGamma(expr: Expr) = expr.vars.map(v => v.toGamma).conjunct
    // Bool.True // expr.vars.map(v => BinOp("&&", v.toGamma, state.getL(v))).conjunct
}

