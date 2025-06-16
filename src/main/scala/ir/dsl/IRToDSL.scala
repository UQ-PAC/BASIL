package ir.dsl

import scala.collection.immutable.{SortedMap, ArraySeq}
import ir.*

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

  def parTransitionSystems(p: Program, p1: Procedure, p2: Procedure) = {
    val entryName = p1.name + "_P_ENTRY"
    val eproc = EventuallyProcedure(
      p1.procName + "_par_" + p2.procName,
      p1.formalInParam.toSeq.map(localVarToTuple).to(SortedMap),
      p1.formalOutParam.toSeq.map(localVarToTuple).to(SortedMap),
      Seq(block(entryName, goto(p1.entryBlock.get.label, p2.entryBlock.get.label))) ++ (p1.blocks ++ p2.blocks).toSet
        .map(convertBlock)
        .to(ArraySeq),
      Some(entryName),
      p2.returnBlock.map(_.label),
      p1.address
    )

    val n = eproc.copy(blocks = eproc.blocks).resolve(p)
    p.procedures.addOne(n)
    n
  }

  def sequenceTransitionSystems(p: Program, p1: Procedure, p2: Procedure) = {
    val eproc = EventuallyProcedure(
      p1.procName + "_seq_" + p2.procName,
      p1.formalInParam.toSeq.map(localVarToTuple).to(SortedMap),
      p1.formalOutParam.toSeq.map(localVarToTuple).to(SortedMap),
      (ArraySeq(p1.entryBlock.get) ++ ((p1.blocks ++ p2.blocks).toSet -- Set(p1.entryBlock.get, p1.returnBlock.get)))
        .map(convertBlock)
        .to(ArraySeq),
      p1.entryBlock.map(_.label),
      p2.returnBlock.map(_.label),
      p1.address
    )

    val exit1 =
      convertBlock(p1.returnBlock.get).copy(j = EventuallyGoto(Seq(DelayNameResolve(p2.entryBlock.get.label))))

    val n = eproc.copy(blocks = eproc.blocks ++ Seq(exit1)).resolve(p)
    p.procedures.addOne(n)
    n
  }

  def keyToString[T](x: (Variable, T)): (String, T) = (x(0).name, x(1))
  def localVarToTuple(x: LocalVar) = (x.name, x.irType)

  def convertJump(x: Jump): EventuallyJump = x match {
    case Unreachable(label) => EventuallyUnreachable(label)
    case Return(label, out) => EventuallyReturn(out.map(keyToString).to(ArraySeq), label)
    case GoTo(targs, label) => EventuallyGoto(targs.map(t => DelayNameResolve(t.label)).toList, label)
  }

  def convertNonCallStatement(x: NonCallStatement): EventuallyStatement = clonedStmt(x)

  def convertCallStatement(x: Call): EventuallyStatement = x match {
    case DirectCall(targ, outs, actuals, label) =>
      // XXX: be aware of ordering, .map() on a SortedMap may return a HashMap.
      directCall(outs.to(ArraySeq).map(keyToString), targ.name, actuals.to(ArraySeq).map(keyToString), label)
    case IndirectCall(targ, label) => indirectCall(targ)
  }

  def convertStatement(x: Statement) = x match {
    case x: NonCallStatement => convertNonCallStatement(x)
    case x: Call => convertCallStatement(x)
  }

  def convertBlock(x: Block) =
    EventuallyBlock(x.label, x.statements.to(ArraySeq).map(convertStatement), convertJump(x.jump), x.address)

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
