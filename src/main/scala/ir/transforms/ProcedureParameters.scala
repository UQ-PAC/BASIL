package ir.transforms
import analysis.{TwoElement, TwoElementBottom, TwoElementTop}
import ir.cilvisitor.*
import ir.{CallGraph, *}
import specification.Specification
import translating.PrettyPrinter
import util.{DebugDumpIRLogger, Logger}

import java.io.File
import scala.collection.{immutable, mutable}

import collection.immutable.SortedMap

case class FunSig(inArgs: List[GlobalVar], outArgs: List[GlobalVar])

def R(n: Int) = {
  Register(s"R$n", 64)
}

val builtinSigs: Map[String, FunSig] = Map(
  "free" -> FunSig(List(R(0)), List()),
  "#free" -> FunSig(List(R(0)), List()),
  "malloc" -> FunSig(List(R(0)), List(R(0))),
  "calloc" -> FunSig(List(R(0), R(1)), List(R(0))),
  "realloc" -> FunSig(List(R(0), R(1)), List(R(0))),
  "reallocarray" -> FunSig(List(R(0), R(1), R(2)), List(R(0))),
  "strlen" -> FunSig(List(R(0)), List(R(0))),
  "strchr" -> FunSig(List(R(0), R(1)), List(R(0))),
  "strlcpy" -> FunSig(List(R(0), R(1), R(2)), List(R(0))),
  "memset" -> FunSig(List(R(0), R(1), R(2)), List(R(0))),
  "strlcat" -> FunSig(List(R(0), R(1), R(2)), List(R(0))),
  "strdup" -> FunSig(List(R(0)), List(R(0))),
  "strndup" -> FunSig(List(R(0), R(1)), List(R(0))),
  "assert" -> FunSig(List(R(0)), List()),
  "exit" -> FunSig(List(R(0)), List()),
  // https://refspecs.linuxfoundation.org/LSB_1.3.0/gLSB/gLSB/baselib---assert-fail-1.html
  "__assert_fail" -> FunSig(List(R(0), R(1), R(2), R(3)), List()),
  "__stack_chk_fail" -> FunSig(List(), List()),
  // "__printf_chk" -> FunSig(List(R(0), R(1)), List(R(0))),
  "__syslog_chk" -> FunSig(List(R(0)), List()),
  "indirect_call_launchpad" -> indirectCallFunsig,
  "__VERIFIER_assert" -> FunSig(List(R(0)), List()),
  "__VERIFIER_assume" -> FunSig(List(R(0)), List()),
  "__VERIFIER_error" -> FunSig(List(), List())
)

def fnsigToBinding(f: FunSig) = (
  f.inArgs.map(a => LocalVar(a.name + "_in", a.getType) -> LocalVar(a.name, a.getType)),
  f.outArgs.map(a => LocalVar(a.name + "_out", a.getType) -> LocalVar(a.name, a.getType))
)

def externalIn(name: String): Map[LocalVar, Variable] = {
  (builtinSigs.get(name)).map(fnsigToBinding).map(_._1) match {
    case Some(x) => x.toMap
    case None =>
      ((0 to 30).toSet -- (19 to 28).toSet)
        .map(i => LocalVar(s"R${i}_in", BitVecType(64)) -> LocalVar(s"R$i", BitVecType(64)))
        .toMap
  }
}
def externalOut(name: String): Map[LocalVar, Variable] = {
  (builtinSigs.get(name)).map(fnsigToBinding).map(_._2) match {
    case Some(x) => x.toMap
    case None =>
      ((0 to 30).toSet -- (19 to 28).toSet)
        .map(i => LocalVar(s"R${i}_out", BitVecType(64)) -> LocalVar(s"R$i", BitVecType(64)))
        .toMap
  }
}

def externalCallReads(name: String) = {
  externalIn(name).map(_._2).map {
    case (l: LocalVar) => Register(l.name, 64)
    case r: GlobalVar => r
  }
}

def externalCallWrites(name: String) = {
  externalIn(name).map(_._2).map {
    case (l: LocalVar) => Register(l.name, 64)
    case r: GlobalVar => r
  }
}

object DefinedOnAllPaths {

  class DefinitelyDefined extends AbstractDomain[Set[Variable]] {

    def bot = Set()
    def top = ???

    def join(a: Set[Variable], b: Set[Variable], c: Block) = {
      a.intersect(b)
    }

    override def init(b: Block): Set[Variable] =
      if (b.parent.entryBlock.contains(b)) then b.parent.formalInParam.toSet else bot

    def transfer(st: Set[Variable], c: Command) = c match {
      case a: Assign => st ++ a.assignees
      case _ => st
    }
  }

  def proc(p: Procedure) = {
    p.returnBlock.toSet.flatMap(rb => {
      val s = transforms.worklistSolver(DefinitelyDefined())
      val (beforeblock, afterblock) = s.solveProc(p, false)
      afterblock(rb)
    })
  }
}

def liftProcedureCallAbstraction(ctx: util.IRContext): util.IRContext = {
  val ns = liftProcedureCallAbstraction(ctx.program, Some(ctx.specification)).get
  ctx.copy(specification = ns)
}

def liftProcedureCallAbstraction(program: Program, spec: Option[Specification]): Option[Specification] = {

  transforms.clearParams(program)

  val mainNonEmpty = program.mainProcedure.blocks.nonEmpty
  val mainHasReturn = program.mainProcedure.returnBlock.isDefined
  val mainHasEntry = program.mainProcedure.entryBlock.isDefined

  val liveVars = if (mainNonEmpty && mainHasEntry && mainHasReturn) {
    analysis.InterLiveVarsAnalysis(program).analyze()
  } else {
    Logger.error(s"Empty live vars $mainNonEmpty $mainHasReturn $mainHasEntry")
    Map.empty
  }
  transforms.applyRPO(program)

  val liveLab = () =>
    liveVars.collect { case (b: Block, r) =>
      b -> {
        val live = r.toList.collect { case (v, TwoElementTop) =>
          v
        }
        val dead = r.toList.collect { case (v, TwoElementBottom) =>
          v
        }
        val livel = live.map(_.name).toList.sorted.mkString(", ")
        // val deadl = dead.map(_.name).toList.sorted.mkString(", ")
        s"Live: $livel"
      }
    }.toMap

  DebugDumpIRLogger.writeToFile(
    File(s"live-vars.il"),
    PrettyPrinter.pp_prog_with_analysis_results(liveLab(), Map(), program, x => x)
  )

  val params = inOutParams(program, liveVars)

  // functions for which we don't know their behaviour and assume they modify all registers
  val external = program.collect {
    case b: Procedure if b.blocks.isEmpty => b.name
  }.toSet

  val formalParams = SetFormalParams(params, external)
  visit_prog(formalParams, program)
  val actualParams = SetActualParams(formalParams.mappingInparam, formalParams.mappingOutparam, external)
  visit_prog(actualParams, program)

  while (removeDeadInParams(program)) {}

  program.procedures.foreach(SpecFixer.updateInlineSpec(formalParams.mappingInparam, formalParams.mappingOutparam))

  spec.map(s => {
    SpecFixer.specToProcForm(s, formalParams.mappingInparam, formalParams.mappingOutparam)
  })
}

def clearParams(p: Program) = {

  class RemoveCallParams extends CILVisitor {
    override def vstmt(s: Statement) = s match {
      case d: DirectCall =>
        ChangeTo(List(DirectCall(d.target, d.label, immutable.SortedMap.empty, immutable.SortedMap.empty)))
      case _ => SkipChildren()
    }
    override def vjump(j: Jump) = {
      j match {
        case d: Return =>
          d.outParams = SortedMap()
        case _ => ()
      }
      SkipChildren()
    }
  }

  for (c <- p) {
    c match {
      case proc: Procedure => {
        proc.formalInParam.clear
        proc.formalOutParam.clear
        proc.inParamDefaultBinding = immutable.SortedMap.empty
        proc.outParamDefaultBinding = immutable.SortedMap.empty
      }
      case _ => ()
    }
  }

  visit_prog(RemoveCallParams(), p)
}

def collectVariables(p: Procedure): (Set[Variable], Set[Variable]) = {
  val lvars = p.blocks.toSet.flatMap(_.statements.flatMap(s => {
    s match {
      case LocalAssign(l, _, _) => Set(l)
      case DirectCall(t, o, _, _) => o.toSet.map(_._2)
      case _ => Set()
    }
  })) ++ p.blocks
    .map(_.jump)
    .collect { case r: Return =>
      r.outParams.toSet.map(_._1)
    }
    .flatten
  val rvars = p.blocks.toSet.flatMap(_.statements.flatMap(s => {
    s match {
      case LocalAssign(l, r, _) => r.variables
      case Assume(l, _, _, _) => l.variables
      case Assert(l, _, _) => l.variables
      case MemoryStore(m, i, v, _, _, _) => i.variables ++ v.variables
      case MemoryLoad(lhs, m, index, endian, size, label) => index.variables ++ Seq(lhs)
      case IndirectCall(l, _) => Set(l)
      case DirectCall(t, o, l, _) => l.toSet.flatMap(_._2.variables)
      case _ => Set()
    }
  }))

  (lvars.toSet, rvars.toSet)
}

class SetFormalParams(
  val inoutparams: Map[Procedure, (Set[Variable], Set[Variable])],
  val externalFunctions: Set[String]
) extends CILVisitor {
  // expects programs to be in single return form

  var mappingOutparam = Map[Procedure, Map[LocalVar, Variable]]()
  var mappingInparam = Map[Procedure, Map[LocalVar, Variable]]()

  override def vproc(p: Procedure) = {
    if (externalFunctions.contains(p.name) || p.isExternal.contains(true)) {
      p.formalInParam = mutable.SortedSet.from(externalIn(p.procName).map(_._1))
      p.formalOutParam = mutable.SortedSet.from(externalOut(p.procName).map(_._1))
      p.inParamDefaultBinding = immutable.SortedMap.from(externalIn(p.procName))
      p.outParamDefaultBinding = immutable.SortedMap.from(externalOut(p.procName))
      mappingInparam = mappingInparam.updated(p, externalIn(p.procName))
      mappingOutparam = mappingOutparam.updated(p, externalOut(p.procName))
      SkipChildren()
    } else {
      val (lvars, rvars) = collectVariables(p)

      val in = IRWalk.firstInProc(p)
      if (in.isDefined) {
        val inparams = inoutparams(p)._1.map(v => (LocalVar(v.name + "_in", v.getType), LocalVar(v.name, v.getType)))
        p.formalInParam.addAll(inparams.map(_._1))
        mappingInparam = mappingInparam.updated(p, inparams.toMap)
        p.inParamDefaultBinding = immutable.SortedMap.from(inparams)
      }

      val outparams = inoutparams(p)._2.map(v => LocalVar(v.name + "_out", v.getType) -> LocalVar(v.name, v.getType))
      p.formalOutParam = mutable.SortedSet.from(outparams.map(_._1))
      mappingOutparam = mappingOutparam.updated(p, outparams.toMap)
      p.outParamDefaultBinding = immutable.SortedMap.from(outparams)

      SkipChildren()
    }
  }
}

object ReadWriteAnalysis {
  sealed trait RW {
    def getRWSet: Option[RWSet] = {
      this match {
        case Top => None
        case r: RWSet => Some(r)
      }
    }
    def map[B](f: RW => RW) = {
      f(this)
    }
  }
  case class RWSet(reads: Set[Variable], writes: Set[Variable]) extends RW
  case object Top extends RW

  def onlyGlobal(r: RWSet) =
    r.copy(reads = r.reads.filterNot(_.isInstanceOf[LocalVar]), writes = r.writes.filterNot(_.isInstanceOf[LocalVar]))

  type st = Map[Procedure, RW]

  def addReads(r: Iterable[Variable])(i: RW) = {
    i match {
      case Top => Top
      case i: RWSet => i.copy(reads = i.reads ++ r)
    }
  }
  def addWrites(w: Iterable[Variable])(i: RW) = {
    i match {
      case Top => Top
      case i: RWSet => i.copy(writes = i.writes ++ w)
    }
  }

  def join(a: RW, b: RW): RW = {
    (a, b) match {
      case (Top, _) => Top
      case (_, Top) => Top
      case (a: RWSet, b: RWSet) => RWSet(a.reads ++ b.reads, a.writes ++ b.writes)
    }
  }

  def processProc(state: st, p: Procedure): RW = {
    p.foldLeft(state(p))((ir, s) => {
      s match {
        case SimulAssign(assigns, _) => {
          val lhs = assigns.map(_._1)
          val rhs = assigns.flatMap(_._2.variables)
          ir.map(addWrites(lhs)).map(addReads(rhs))
        }
        case s: MemoryAssign => {
          ir.map(addWrites(Seq(s.lhs)))
            .map(addReads(s.rhs.variables))
        }
        case s: MemoryLoad => {
          ir.map(addWrites(Seq(s.lhs)))
            .map(addReads(s.index.variables))
        }
        case s: Return => {
          ir.map(addReads(s.outParams.flatMap(_._2.variables)))
        }
        case s: MemoryStore => {
          ir.map(addReads(s.index.variables ++ s.value.variables))
        }
        case s: DirectCall if (s.target.isExternal.contains(true)) => {
          ir.map(addReads(externalCallReads(s.target.procName)))
            .map(addWrites(externalCallWrites(s.target.procName)))
        }
        case s: DirectCall => {
          ir.map(x => join(x, state(s.target)))
            .map(addReads(s.actualParams.flatMap(_._2.variables)))
            .map(addWrites(s.outParams.flatMap(_._2.variables)))
        }
        case s: IndirectCall => Top
        case s: Assert => ir.map(addReads(s.body.variables))
        case s: Assume => ir.map(addReads(s.body.variables))
        case p: Procedure => ir
        case b: Block => ir
        case b: NOP => ir
        case b: Unreachable => ir
        case b: GoTo => ir
      }
    })
  }

  def readWriteSets(p: Program): Map[Procedure, Option[RWSet]] = {
    var state: st = Map[Procedure, RW]().withDefaultValue(RWSet(Set(), Set()))
    val worklist = mutable.Stack.from(p.procedures)

    while (worklist.nonEmpty) {
      val proc = worklist.pop
      val o = state(proc)
      val n = processProc(state, proc)
      if (o != n) {
        worklist.addAll(CallGraph.pred(proc))
        worklist.addAll(CallGraph.succ(proc))
        state = state.updated(proc, n)
      }
    }

    state.map(x => (x._1, x._2.getRWSet))
  }
}

def inOutParams(
  p: Program,
  interLiveVarsResults: Map[CFGPosition, Map[Variable, TwoElement]]
): Map[Procedure, (Set[Variable], Set[Variable])] = {
  val overapprox = ((0 to 31).toSet -- (19 to 28).toSet).map(i => Register(s"R${i}", 64)).toSet[Variable]
  // in: live at entry & in procedure read set

  var readWrites = ReadWriteAnalysis.readWriteSets(p: Program).collect {
    case (p, None) => (p, ReadWriteAnalysis.RWSet(overapprox, overapprox))
    case (p, Some(x)) => (p, ReadWriteAnalysis.onlyGlobal(x))
  }

  val procEnd = p.procedures.map { case p =>
    p -> p.returnBlock.getOrElse(p)
  }.toMap

  val lives: Map[Procedure, (Set[Variable], Set[Variable])] = p.procedures
    .map(p => {
      val in = (interLiveVarsResults.get(p))

      def toLiveSet(p: Option[Map[Variable, TwoElement]]): Set[Variable] = {
        p.map(p => {
          p.collect { case (v, TwoElementTop) =>
            v
          }.toSet
        }).getOrElse(overapprox)
      }

      // live after any call to procedure
      val out = p.incomingCalls().map(_.successor).map(v => toLiveSet(interLiveVarsResults.get(v))).toSet.flatten

      p -> (toLiveSet(in), out)
    })
    .toMap

  val alwaysReturnParams = (0 to 7).map(i => Register(s"R$i", 64))

  val pc = Register("_PC", 64)

  val inout = readWrites.collect {
    case (proc, rws) if p.mainProcedure == proc => {
      // no callers of main procedure so keep the whole read/write set
      // of registers

      val outParams = (overapprox.intersect(DefinedOnAllPaths.proc(proc)))
      val inParams = lives(proc)._1 ++ (if proc.procName == "main" then Set(R(0), R(1)) else Set())
      proc -> (inParams + pc, outParams + pc)
    }
    case (proc, rws) => {
      val liveStart = lives(proc)._1
      val liveEnd = lives(proc)._2 ++ alwaysReturnParams

      val outParams = liveEnd.intersect(rws.writes)
      val inParams = liveStart ++ (if proc.procName == "main" then Set(R(0), R(1)) else Set())
      proc -> (inParams + pc, outParams + pc)
    }
  }.toMap

  // callgraph fixed point

  val defUseDom = DefUseEntryDomain()
  val defUseSolver = worklistSolver(defUseDom)
  val defUses = lives.keySet.map(k => k -> defUseSolver.solveProc(k)).toMap

  def reachesEntry(v: Variable, s: Command) = {
    val stmts = s.parent.statements.takeWhile(_ != s)
    val r = for {
      init <- defUses(s.parent.parent)._1.get(s.parent)
      r <- stmts.foldLeft(init)(defUseDom.transfer).get(v)
    } yield (r.contains(Def.Entry))
    r.getOrElse(true)
    true
  }

  var newParams = inout
  var oldParams = Map[Procedure, (Set[Variable], Set[Variable])]()

  while (newParams != oldParams) {
    oldParams = newParams

    for (proc <- p.procedures.filter(newParams.contains)) {
      val origIn = oldParams(proc)._1
      val origOut = oldParams(proc)._2

      val calls = proc.collect { case c: DirectCall =>
        c
      }

      val modifiedFromCall =
        proc.calls.flatMap(p => oldParams.get(p).toSet.flatMap(_._2)).filterNot(_.isInstanceOf[LocalVar])

      val liveFromCall = {
        (for {
          c <- calls
          res <- oldParams.get(c.target)
          globs = res._1.filterNot(_.isInstanceOf[LocalVar])
          vars = globs.filter(x => reachesEntry(x, c))
        } yield (vars)).toSet.flatten
      }

      readWrites = readWrites.updated(proc, readWrites(proc).copy(writes = readWrites(proc).writes ++ modifiedFromCall))
      val writes = readWrites(proc).writes

      val newOut = origOut ++ (if (proc == p.mainProcedure) then Seq() else lives(proc)._2.intersect(writes))
      val liveFromReturn = newOut.filter(i => proc.returnBlock.map(b => reachesEntry(i, b.jump)).getOrElse(true))

      // filtering by reaching entry does not seem to have much effect
      val extraLive = (liveFromReturn ++ liveFromCall)
      val newIn = origIn ++ extraLive

      newParams = newParams.updated(proc, (newIn, newOut))
    }

  }

  // val counts = newParams.toList.map(p => (p._1, p._2._1.size, p._2._2.size))
  // for ((p, i, o) <- counts.sortBy(_._1.name)) {
  //   Logger.info(s"${p.name} in $i out $o")
  // }

  newParams.withDefaultValue((overapprox, overapprox))
}

class SetActualParams(
  val inBinding: Map[Procedure, Map[LocalVar, Variable]],
  val outBinding: Map[Procedure, Map[LocalVar, Variable]],
  val externalFunctions: Set[String]
) extends CILVisitor {
  // expects programs to be in single return form
  var currStmt: Option[Statement] = None

  override def vproc(p: Procedure) = {
    val incoming =
      p.formalInParam.toList.flatMap(param =>
        inBinding.get(p).flatMap(_.get(param)).map(p => LocalAssign(p, param)).toList
      )
    p.entryBlock.foreach(b => b.statements.prependAll(incoming))
    DoChildren()
  }

  override def vstmt(s: Statement) = {
    currStmt = Some(s)
    s match {
      // case d: DirectCall if !externalFunctions.contains(d.target.name) => {
      //   // we have changed the parameter-passed variable to locals so we have LocalVar(n) -> LocalVar(n)
      //   for (binding <- inBinding.get(d.target)) {
      //     if (externalFunctions.contains(d.target.name)) {
      //       d.actualParams = SortedMap.from(binding)
      //     } else {
      //       d.actualParams = d.actualParams ++ SortedMap.from(binding)
      //     }
      //   }
      //   for (binding <- outBinding.get(d.target)) {
      //     d.outParams = SortedMap.from(binding)
      //   }
      // }
      case d: DirectCall => {
        d.actualParams = SortedMap.from(inBinding(d.target))
        d.outParams = SortedMap.from(outBinding(d.target))
      }
      case _ => ()
    }
    DoChildren()
  }

  override def vjump(j: Jump) = {
    j match {
      case r: Return => {
        r.outParams = SortedMap.from(outBinding(r.parent.parent))
        DoChildren()
      }
      case _ => DoChildren()
    }
  }

  override def vlvar(v: Variable) = {
    ChangeTo(LocalVar(v.name, v.getType))
  }
  override def vrvar(v: Variable) = {
    ChangeTo(LocalVar(v.name, v.getType))
  }

}

object SpecFixer {
  import boogie.*

  class ExprToOld(varInPre: Map[String, Variable], varInPost: Map[String, Variable], initIsPost: Boolean = false)
      extends CILVisitor {

    var isPost = initIsPost

    override def vexpr(e: Expr) = e match {
      case OldExpr(inner) if !isPost => {
        throw Exception("nested Old or Old in single-state context")
      }
      case OldExpr(inner) if isPost => {
        isPost = false
        ChangeDoChildrenPost(
          inner,
          e => {
            isPost = true
            e
          }
        )
      }
      case _ => DoChildren()
    }

    def changeVar(v: Variable) = {
      val repl = v match {
        case l: Variable if isPost && varInPost.contains(l.name) => varInPost(l.name)
        case l: Variable if varInPre.contains(l.name) => varInPre(l.name)
        case o => o
      }
      ChangeTo(repl)
    }

    override def vlvar(v: Variable) = changeVar(v)
    override def vrvar(v: Variable) = changeVar(v)

  }

  def convVarToOldExpr(varInPre: Map[String, Variable], varInPost: Map[String, Variable], isPost: Boolean = false)(
    b: Expr
  ): Expr = {
    val visitor = ExprToOld(varInPre, varInPost, isPost)
    visit_expr(visitor, b)
  }

  def convVarToOld(
    varInPre: Map[String, String],
    varInPost: Map[String, String],
    isPost: Boolean = false,
    makeLocal: Boolean = false
  )(b: BExpr): BExpr = {
    val varToOld = convVarToOld(varInPre, varInPost, isPost, makeLocal)
    val preVars = varInPre.values.toSet
    val postVars = varInPost.values.toSet
    b match {
      case b: BVariable => {
        if isPost && varInPost.contains(b.name) then
          BVariable(varInPost(b.name), b.getType, if makeLocal then Scope.Local else b.scope)
        else if !isPost && varInPre.contains(b.name) then
          BVariable(varInPre(b.name), b.getType, if makeLocal then Scope.Local else b.scope)
        else if !isPost && preVars.contains(b.name) then b
        else if isPost && postVars.contains(b.name) then b
        else {
          val st = if isPost then "post" else "pre"
          val ps = if isPost then "out" else "in"
          val m = if isPost then varInPost else varInPre
          throw Exception(s"$b var in $st state context but not in ${ps}-param set: $m")
        }
      }
      case b: BLiteral => b
      case b: BVExtract => b.copy(body = varToOld(b.body))
      case b: BVRepeat => b.copy(body = varToOld(b.body))
      case b: BVZeroExtend => b.copy(body = varToOld(b.body))
      case b: BVSignExtend => b.copy(body = varToOld(b.body))
      case b: BFunctionCall => b.copy(args = b.args.map(varToOld))
      case b: UnaryBExpr => b.copy(arg = varToOld(b.arg))
      case b: BinaryBExpr => b.copy(arg1 = varToOld(b.arg1), arg2 = varToOld(b.arg2))
      case b: IfThenElse => IfThenElse(varToOld(b.guard), varToOld(b.thenExpr), varToOld(b.elseExpr))
      case b: BQuantifierExpr => b
      case b: Old => {
        if (isPost) {
          Old(convVarToOld(varInPre, varInPost, false, makeLocal)(b.body))
        } else {
          throw Exception("Illegal nested or non-relation Old()")
        }
      }
      case b: MapAccess => b.copy(index = varToOld(b.index))
      case b: MapUpdate => b.copy(index = varToOld(b.index), value = varToOld(b.value))
      case b: BByteExtract => b.copy(value = varToOld(b.value), offset = varToOld(b.offset))
      case b: BInBounds => b.copy(base = varToOld(b.base), len = varToOld(b.len), i = varToOld(b.i))
      case b: BMemoryLoad => b.copy(index = varToOld(b.index))
      case b: BMemoryStore => b.copy(index = varToOld(b.index), value = varToOld(b.value))
      case b: BDirectExpr => b
      case b: GammaLoad => b.copy(index = varToOld(b.index))
      case b: GammaStore => b.copy(index = varToOld(b.index), value = varToOld(b.value))
      case b: L => b.copy(index = varToOld(b.index))
      case b: AssocBExpr => b.copy(arg = b.arg.map(varToOld))
      case b: SpecVar => b
      case b: BVar =>
        // because BVar is unsealed
        throw Exception(s"unexpected var: $b ${b.getClass.getSimpleName}")
    }
  }

  def updateInlineSpec(
    mappingInparam: Map[Procedure, Map[LocalVar, Variable]],
    mappingOutparam: Map[Procedure, Map[LocalVar, Variable]]
  )(p: Procedure) = {

    def toNameMapping(v: Map[LocalVar, Variable]): Map[String, String] = {
      v.map(v => (v._2.name, v._1.name)) ++ v.map(v => ("Gamma_" + v._2.name, "Gamma_" + v._1.name))
    }

    val varToInVar = mappingInparam.map(p => (p._1 -> toNameMapping(p._2)))
    val varToOutVar = mappingOutparam.map(p => (p._1 -> toNameMapping(p._2)))

    p.requires = p.requires.map(convVarToOld(varToInVar(p), varToOutVar(p), isPost = false, makeLocal = true))
    p.ensures = p.ensures.map(convVarToOld(varToInVar(p), varToOutVar(p), isPost = true, makeLocal = true))

    def toVarMapping(v: Map[LocalVar, Variable]): Map[String, Variable] = {
      v.map(v => (v._2.name, v._1))
    }

    p.requiresExpr =
      p.requiresExpr.map(convVarToOldExpr(toVarMapping(mappingInparam(p)), toVarMapping(mappingOutparam(p)), false))
    p.ensuresExpr =
      p.ensuresExpr.map(convVarToOldExpr(toVarMapping(mappingInparam(p)), toVarMapping(mappingOutparam(p)), true))

  }

  def specToProcForm(
    spec: Specification,
    mappingInparam: Map[Procedure, Map[LocalVar, Variable]],
    mappingOutparam: Map[Procedure, Map[LocalVar, Variable]]
  ): Specification = {

    def toNameMapping(v: Map[LocalVar, Variable]): Map[String, String] = {
      v.map(v => (v._2.name, v._1.name)) ++ v.map(v => ("Gamma_" + v._2.name, "Gamma_" + v._1.name))
    }
    val varToInVar: Map[String, Map[String, String]] = mappingInparam.map(p => (p._1.procName -> toNameMapping(p._2)))
    val varToOutVar: Map[String, Map[String, String]] = mappingOutparam.map(p => (p._1.procName -> toNameMapping(p._2)))

    val ns = spec.copy(subroutines = spec.subroutines.map(s => {
      if (s.requires.nonEmpty || s.ensures.nonEmpty) {
        val in = varToInVar(s.name)
        val out = varToOutVar(s.name)
        s.copy(
          requires = s.requires.map(convVarToOld(in, out, false, true)),
          ensures = s.ensures.map(convVarToOld(in, out, true, true))
        )
      } else {
        s
      }
    }))
    ns
  }
}
