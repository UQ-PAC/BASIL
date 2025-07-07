package ir.dsl

import ir.*

import scala.collection.immutable.{ArraySeq, SortedMap}

/**
 * Basil IR to DSL
 * ===============
 * This file implements conversion of Basil IR to DSL structures.
 * The goal of this conversion is to easily support *cloning* of the Basil IR by
 * first converting it to a DSL, then resolving that DSL. The resulting new
 * Basil IR structure can then be used within a new program or appended into the
 * original program.
 */

object IRToDSL {

  def keyToString[T](x: (Variable, T)): (String, T) = (x(0).name, x(1))
  def localVarToTuple(x: LocalVar) = (x.name, x.irType)

  def convertJump(x: Jump): EventuallyJump = x match {
    case Unreachable(label) => EventuallyUnreachable(label, x.comment)
    case Return(label, out) => EventuallyReturn(out.map(keyToString).to(ArraySeq), label, x.comment)
    case GoTo(targs, label) => EventuallyGoto(targs.map(t => DelayNameResolve(t.label)).toList, label, x.comment)
  }

  def convertNonCallStatement(x: NonCallStatement): EventuallyStatement = clonedStmt(x)

  def convertCallStatement(x: Call): EventuallyStatement = x match {
    case DirectCall(targ, outs, actuals, label) =>
      // XXX: be aware of ordering, .map() on a SortedMap may return a HashMap.
      directCall(outs.to(ArraySeq).map(keyToString), targ.name, actuals.to(ArraySeq).map(keyToString), label)
        .copy(comment = x.comment)
    case IndirectCall(targ, label) => indirectCall(targ).copy(label = label, comment = x.comment)
  }

  def convertStatement(x: Statement) = x match {
    case x: NonCallStatement => convertNonCallStatement(x)
    case x: Call => convertCallStatement(x)
  }

  def convertBlock(x: Block) =
    EventuallyBlock(x.label, x.statements.to(ArraySeq).map(convertStatement), convertJump(x.jump), x.meta)

  def convertProcedure(x: Procedure) =
    EventuallyProcedure(
      x.procName,
      x.formalInParam.toSeq.map(localVarToTuple).to(SortedMap),
      x.formalOutParam.toSeq.map(localVarToTuple).to(SortedMap),
      x.blocks.map(convertBlock).to(ArraySeq),
      x.entryBlock.map(_.label),
      x.returnBlock.map(_.label),
      x.address
    )

  def convertProgram(x: Program) =
    val others = x.procedures.filter(_ != x.mainProcedure).map(convertProcedure)
    EventuallyProgram(convertProcedure(x.mainProcedure), others.to(ArraySeq), x.initialMemory.values)

}
