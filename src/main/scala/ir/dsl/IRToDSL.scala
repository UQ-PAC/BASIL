package ir.dsl

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

  def keyToString[T](x: (Variable, T)): (String, T) = (x(0).name, x(1))
  def localVarToTuple(x: LocalVar) = (x.name, x.irType)

  def convertJump(x: Jump): EventuallyJump = x match {
    case Unreachable(label) => unreachable
    case Return(label, out) => ret(out.map(keyToString).toArray: _*)
    case GoTo(targs, label) => goto(targs.map(_.label).toArray: _*)
  }

  def cloneStatement(x: NonCallStatement): NonCallStatement = x match {
    case LocalAssign(a, b, c) => LocalAssign(a, b, c)
    case MemoryStore(a, b, c, d, e, f) => MemoryStore(a, b, c, d, e, f)
    case MemoryLoad(a, b, c, d, e, f) => MemoryLoad(a, b, c, d, e, f)
    case x: NOP => NOP(x.label) // FIXME: no unapply for NOP atm
    case Assert(a, b, c) => Assert(a, b, c)
    case Assume(a, b, c, d) => Assume(a, b, c, d)
  }

  def convertNonControlStatement(x: NonCallStatement): EventuallyStatement =
    ResolvableStatement(x)

  def convertControlStatement(x: CallStatement): EventuallyStatement = x match {
    case DirectCall(targ, outs, actuals, label) =>
      // XXX: be aware of ordering, .map() on a SortedMap may return a HashMap.
      directCall(outs.toArray.map(keyToString), targ.name, actuals.toArray.map(keyToString): _*)
    case IndirectCall(targ, label) => indirectCall(targ)
  }

  def convertCommand(x: Command) = x match {
    case x: Jump => convertJump(x)
    case x: NonCallStatement => convertNonControlStatement(x)
    case x: CallStatement => convertControlStatement(x)
  }

  def convertBlock(x: Block) =
    block(x.label, (x.statements ++ Iterable(x.jump)).map(convertCommand).toArray: _*)

  def convertProcedure(x: Procedure) =
    proc(
      x.name,
      x.formalInParam.toSeq.map(localVarToTuple),
      x.formalOutParam.toSeq.map(localVarToTuple),
      x.blocks.map(convertBlock).toArray: _*
    )

  def convertProgram(x: Program) =
    val others = x.procedures.filter(_ != x.mainProcedure).map(convertProcedure)
    EventuallyProgram(convertProcedure(x.mainProcedure), others.toArray: _*)

}
