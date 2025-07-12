package ir.transforms
import ir.*
import ir.cilvisitor.*
import ir.dsl.*

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

private val counter = util.Counter()

def memoise[K, V](f: K => V): K => V = {
  val rn = mutable.Map[K, V]()

  def fun(arg: K): V = {
    if (!rn.contains(arg)) {
      rn(arg) = f(arg)
    }

    rn(arg)
  }
  fun
}

def renameBlock(s: String): String = {
  s + "_" + (counter.next())
}

class VarRenamer(proc: Procedure) extends CILVisitor {

  def doRename(v: Variable): Variable = v match {
    case l: LocalVar if l.name.endsWith("_in") => {
      val name = l.name.stripSuffix("_in")
      proc.getFreshSSAVar(name, l.getType)
    }
    case l: LocalVar if l.index != 0 =>
      proc.getFreshSSAVar(l.varName, l.getType)
    case _ => v
  }
  val memoed = memoise(doRename)

  def rename(v: Variable) = {
    memoed(v)
  }

  override def vlvar(v: Variable) = {
    ChangeTo(rename(v))
  }

  override def vrvar(v: Variable) = {
    ChangeTo(rename(v))
  }

}

def convertJumpRenaming(blockName: String => String, varName: CILVisitor, x: Jump): EventuallyJump = x match {
  case GoTo(targs, label) => EventuallyGoto(targs.map(t => DelayNameResolve(blockName(t.label))).toList, label)
  case Unreachable(label) => EventuallyUnreachable(label)
  case Return(label, out) =>
    EventuallyReturn(
      out.toList
        .map { case (v: Variable, e: Expr) =>
          ((v.name, visit_expr(varName, e)))
        }
        .to(ArraySeq),
      label
    )
}

def keyToString[T](varRenamer: CILVisitor)(x: (Variable, Expr)): (String, Expr) =
  (x(0).name, visit_expr(varRenamer, x(1)))

def convertStatementRenaming(varRenamer: CILVisitor)(x: Statement): EventuallyStatement = x match {
  case DirectCall(targ, outs, actuals, label) =>
    directCall(
      outs.to(ArraySeq).map(v => (v(0).name, visit_rvar(varRenamer, v(1)))),
      targ.name,
      actuals.to(ArraySeq).map(keyToString(varRenamer)),
      label
    )
  case IndirectCall(targ, label) => indirectCall(visit_rvar(varRenamer, targ))
  case x: NonCallStatement =>
    CloneableStatement(visit_stmt(varRenamer, cloneStatement(x)).head.asInstanceOf[NonCallStatement])
}

def convertBlockRenaming(varRenamer: CILVisitor, blockName: String => String)(x: Block) = {
  EventuallyBlock(
    blockName(x.label),
    x.statements.to(ArraySeq).map(convertStatementRenaming(varRenamer)),
    convertJumpRenaming(blockName, varRenamer, x.jump),
    x.meta
  )
}

/**
  * Inline a procedure call to the calling procedure;
  *
  * - maintains param form
  * - maintain dsa form if both procedures are in dsa form
  *
  * require invariant.SingleCallBlockEnd
  */
def inlineCall(prog: Program, c: DirectCall): Unit = {
  require(c.target.entryBlock.isDefined)
  require(c.target.returnBlock.isDefined)

  val proc = c.parent.parent
  val block = c.parent
  val target = c.target
  // rename ssa variables to maintain DSA in the new procedure
  // rename block labels to avoid creating conflicts if inlining the same function multiple times
  val varRenamer = VarRenamer(proc)
  val blockRenamer: String => String = memoise(renameBlock)

  val entry = target.entryBlock.get
  val returnBlock = target.returnBlock.get

  // the internal blocks to the target
  val internalBlocks =
    (target.blocks.toSet -- Seq(entry, returnBlock)).map(convertBlockRenaming(varRenamer, blockRenamer))

  // clone the target procedure and apply the renaming
  val (entryTempBlock, entryResolver) = convertBlockRenaming(varRenamer, blockRenamer)(entry).makeResolver
  val eventuallyReturnBlock = convertBlockRenaming(varRenamer, blockRenamer)(returnBlock)
  val afterCallBlock = block.createBlockAfter("_inlineret")
  proc.addBlock(entryTempBlock)
  block.replaceJump(GoTo(entryTempBlock))

  // replace return in target with a jump to the aftercall block
  val (returnTemp, resolveReturnBlock) = eventuallyReturnBlock.copy(j = unreachable).makeResolver
  proc.addBlock(returnTemp)

  // resolve internal call blocks
  val resolvers = internalBlocks.map(_.makeResolver)
  resolvers.foreach { case (block, _) => proc.addBlock(block) }
  val reso = CachedLabelResolver(prog)
  resolvers.foreach { case (_, resolve) => resolve(reso, proc.name) }

  // remove original call statement
  block.statements.remove(c)
  // link the inlined blocks to the call block and the aftercall block
  entryResolver(reso, proc.name)
  block.replaceJump(GoTo(entryTempBlock))
  resolveReturnBlock(reso, proc.name)
  returnTemp.replaceJump(GoTo(afterCallBlock))

  // assign the actual parameters in the caller to the renamed formal parameters in the entry block
  val targetReturnValues: Map[String, Expr] = eventuallyReturnBlock.j match {
    case r: EventuallyReturn => r.params.toMap
    case _ => throw Exception("returnblock should have a return statement")
  }
  val outAssignments = c.outParams.map { case (formal: LocalVar, lvar: Variable) =>
    LocalAssign(lvar, targetReturnValues(formal.name))
  }
  val inAssignments = c.actualParams.map { case (formal: LocalVar, actual: Expr) =>
    LocalAssign(visit_rvar(varRenamer, formal), actual)
  }
  afterCallBlock.statements.prependAll(outAssignments)
  entryTempBlock.statements.prependAll(inAssignments)

}
