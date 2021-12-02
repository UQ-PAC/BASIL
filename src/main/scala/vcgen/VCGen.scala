package vcgen

import astnodes.exp.{BinOp, Expr, Literal, Var}
import facts.stmt.Assign.{Assign, MemAssign, RegisterAssign}
import facts.stmt.{Assert, CJmpStmt, Stmt}
import translating.FlowGraph

import collection.JavaConverters.*
import astnodes.pred.{Bool, Pred}

object VCGen {
  def genVCs(flowGraph: FlowGraph, state: State): List[Stmt] = {
    flowGraph.setFunctions(flowGraph.getFunctions.asScala.map(func => {
      func
    }).asJava)

    flowGraph.getLines.asScala.flatMap(line =>
      List(line, Assert("-1", genVC(line, state)))
    ).toList
  }

  /**
   * Generate the VC for a given statement
   *
   * @param stmt
   * @return
   */
  def genVC(stmt: Stmt, state: State): Pred = stmt match {
    case cjmpStmt: CJmpStmt => computeGamma(cjmpStmt.getCondition, state)
    case assign: RegisterAssign => ???
    case assign: MemAssign => ???
    // TODO do we treat this like an array / map (or we could convert to vars but that seems like a bad idea)
    // TODO ^ my current thinking is that we specify using the original vars, use the GOT/symbol table to generate a corresponding specification
    // on the memory
    case _: Assert => ??? // There should not be an assert here
    case _ => ???
  }

  def computeGamma(expr: Expr, state: State) = Bool.True // expr.vars.map(v => BinOp("&&", v.toGamma, state.getL(v))).conjunct
}