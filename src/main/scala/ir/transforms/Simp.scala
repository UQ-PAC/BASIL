package ir.transforms
import translating.serialiseIL
import translating.PrettyPrinter.*

import boogie.FuncEntry
import util.SimplifyLogger
import ir.eval.AlgebraicSimplifications
import ir.eval.AssumeConditionSimplifications
import ir.eval.simplifyExprFixpoint
import ir.cilvisitor.*
import ir.*
import scala.collection.mutable
import analysis._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.*
import scala.util.{Failure, Success}
import ExecutionContext.Implicits.global

def getLiveVars(p: Procedure): (Map[Block, Set[Variable]], Map[Block, Set[Variable]]) = {
  val liveVarsDom = IntraLiveVarsDomain()
  val liveVarsSolver = worklistSolver(liveVarsDom)
  liveVarsSolver.solveProc(p, backwards = true)
}

def difftestLiveVars(p: Procedure, compareResult: Map[CFGPosition, Set[Variable]]) = {
  val (liveBefore, liveAfter) = getLiveVars(p)
  var passed = true

  for ((b, s) <- liveBefore) {
    val c = (compareResult(b) == s)
    passed = passed && c
    if (!c) {
      SimplifyLogger.error(
        s"LiveVars unequal ${b.label}: ${compareResult(b)} == $s (differing ${compareResult(b).diff(s)})"
      )
    }
  }
  passed
}

def basicReachingDefs(p: Procedure): Map[Command, Map[Variable, Set[Assign | DirectCall]]] = {
  val (beforeLive, afterLive) = getLiveVars(p)
  val dom = DefUseDomain(beforeLive)
  val solver = worklistSolver(dom)
  // type rtype = Map[Block, Map[Variable, Set[Assign | DirectCall]]]
  val (beforeRes, afterRes) = solver.solveProc(p)

  val merged: Map[Command, Map[Variable, Set[Assign | DirectCall]]] =
    beforeRes
      .flatMap((block, sts) => {
        val b = Seq(IRWalk.firstInBlock(block) -> sts)
        val stmts =
          if (block.statements.nonEmpty) then
            (block.statements.toList: List[Command]).zip(block.statements.toList.tail ++ List(block.jump))
          else List()
        val transferred = stmts
          .foldLeft((sts, List[(Command, Map[Variable, Set[Assign | DirectCall]])]()))((st, s) => {
            // map successor to transferred predecessor
            val x = dom.transfer(st._1, s._1)
            (x, (s._2 -> x) :: st._2)
          })
          ._2
          .toMap
        b ++ transferred
      })
      .toMap
  merged
}

case class DefUse(defined: Map[Variable, Assign])

// map v -> definitions reached here
class DefUseDomain(liveBefore: Map[Block, Set[Variable]]) extends AbstractDomain[Map[Variable, Set[Assign]]] {

  override def transfer(s: Map[Variable, Set[Assign]], b: Command) = {
    b match {
      case a: LocalAssign => s.updated(a.lhs, Set(a))
      case a: MemoryLoad  => s.updated(a.lhs, Set(a))
      case d: DirectCall  => d.outParams.map(_._2).foldLeft(s)((s, r) => s.updated(r, Set(d)))
      case _              => s
    }
  }
  override def top = ???
  def bot = Map[Variable, Set[Assign]]()
  def join(l: Map[Variable, Set[Assign]], r: Map[Variable, Set[Assign]], pos: Block) = {
    l.keySet
      .union(r.keySet)
      .filter(k => liveBefore(pos).contains(k))
      .map(k => {
        k -> (l.get(k).getOrElse(Set()) ++ r.get(k).getOrElse(Set()))
      })
      .toMap
  }

}

class IntraLiveVarsDomain extends PowerSetDomain[Variable] {
  // expected backwards

  def transfer(s: Set[Variable], a: Command): Set[Variable] = {
    a match {
      case a: LocalAssign  => (s - a.lhs) ++ a.rhs.variables
      case a: MemoryLoad   => (s - a.lhs) ++ a.index.variables
      case m: MemoryStore  => s ++ m.index.variables ++ m.value.variables
      case a: Assume       => s ++ a.body.variables
      case a: Assert       => s ++ a.body.variables
      case i: IndirectCall => s + i.target
      case c: DirectCall   => (s -- c.outParams.map(_._2)) ++ c.actualParams.flatMap(_._2.variables)
      case g: GoTo         => s
      case r: Return       => s ++ r.outParams.flatMap(_._2.variables)
      case r: Unreachable  => s
      case n: NOP          => s
    }
  }
}

def removeSlices(p: Program): Unit = {
  p.procedures.foreach(removeSlices)
}

case class LVTerm(v: LocalVar) extends analysis.solvers.Var[LVTerm]

def removeSlices(p: Procedure): Unit = {

  /** if for each variable v there is some i:int such that (i) all its assignments have a ZeroExtend(i, x) and (ii) all
    * its uses have Extract(size(v) - i, 0, v) Then we replace v by a variable of bitvector size (size(v) - i)
    *
    * We check this flow-insensitively and recover precision using DSA form.
    */
  val assignments: Map[LocalVar, Iterable[Assign]] = p
    .collect {
      case a: SingleAssign => Seq(a.lhs -> a)
      case a: DirectCall   => a.assignees.map(e => e -> a)
    }
    .flatten
    .groupBy(_._1)
    .map((k, v) => (k, v.map(_._2).toSet))
    .collect { case (k: LocalVar, v) =>
      (k, v)
    }
  enum HighZeroBits:
    case Bits(n: Int) // (i) and (ii) hold; the n highest bits are redundant
    case False // property is false
    case Bot // don't know anything
  def size(e: Expr) = {
    e.getType match {
      case BitVecType(s) => Some(s)
      case _             => None
    }
  }
  // unify variable uses across direct assignments
  val ufsolver = analysis.solvers.UnionFindSolver[LVTerm]()
  val unioned = assignments.foreach {
    case (lv, LocalAssign(lhs: LocalVar, rhs: LocalVar, _)) => ufsolver.unify(LVTerm(lhs), LVTerm(rhs))
    case _                                                  => ()
  }
  val unifiedAssignments = ufsolver
    .unifications()
    .map {
      case (v @ LVTerm(_), rvs) =>
        v.v -> (rvs.map {
          case LVTerm(rv) =>
            rv
          case _ => ??? /* unreachable */
        }).toSet
      case _ => ??? /* unreachable */
    }
    .map((repr: LocalVar, elems: Set[LocalVar]) =>
      repr -> elems.flatMap(assignments(_).filter(_ match {
        // filter out the direct assignments we used to build the unif class
        case LocalAssign(lhs: LocalVar, rhs: LocalVar, _) if elems.contains(lhs) && elems.contains(rhs) => false
        case _                                                                                          => true
      }))
    )
  // try and find a single extension size for all rhs of assignments to all variables in the assigned equality class
  val varHighZeroBits: Map[LocalVar, HighZeroBits] = assignments.map((v, assigns) =>
    // note: this overapproximates on x := y when x and y may both be smaller than their declared size
    val allRHSExtended = assigns.foldLeft(HighZeroBits.Bot: HighZeroBits)((e, assign) =>
      (e, assign) match {
        case (HighZeroBits.Bot, LocalAssign(_, ZeroExtend(i, lhs), _))                   => HighZeroBits.Bits(i)
        case (b @ HighZeroBits.Bits(ei), LocalAssign(_, ZeroExtend(i, _), _)) if i == ei => b
        case (b @ HighZeroBits.Bits(ei), LocalAssign(_, ZeroExtend(i, _), _)) if i != ei => HighZeroBits.False
        case (b @ HighZeroBits.Bits(ei), m: MemoryLoad)                                  => HighZeroBits.False
        case (b @ HighZeroBits.Bits(ei), m: DirectCall)                                  => HighZeroBits.False
        case (HighZeroBits.False, _)                                                     => HighZeroBits.False
        case (_, other)                                                                  => HighZeroBits.False
      }
    )
    (v, allRHSExtended)
  )
  val varsWithExtend: Map[LocalVar, HighZeroBits] = assignments
    .map((lhs, _) => {
      // map all lhs to the result for their representative
      val rep = ufsolver.find(LVTerm(lhs)) match {
        case LVTerm(r) => r
        case _         => ??? /* unreachable */
      }
      lhs -> varHighZeroBits.get(rep)
    })
    .collect { case (l, Some(x)) /* remove anything we have no information on */ =>
      (l, x)
    }
  class CheckUsesHaveExtend() extends CILVisitor {
    val result: mutable.HashMap[LocalVar, HighZeroBits] =
      mutable.HashMap[LocalVar, HighZeroBits]()
    override def vexpr(v: Expr) = {
      v match {
        case Extract(i, 0, v: LocalVar)
            if size(v).isDefined && result.get(v).contains(HighZeroBits.Bits(size(v).get - i)) =>
          SkipChildren()
        case v: LocalVar => {
          result.remove(v)
          SkipChildren()
        }
        case _ => DoChildren()
      }
    }
    def apply(assignHighZeroBits: Map[LocalVar, HighZeroBits])(p: Procedure): Map[LocalVar, HighZeroBits] = {
      result.clear()
      result.addAll(assignHighZeroBits)
      visit_proc(this, p)
      result.toMap
    }
  }
  val toSmallen = CheckUsesHaveExtend()(varsWithExtend)(p).collect { case (v, HighZeroBits.Bits(x)) =>
    v -> x
  }.toMap
  class ReplaceAlwaysSlicedVars(varHighZeroBits: Map[LocalVar, Int]) extends CILVisitor {
    override def vexpr(v: Expr) = {
      v match {
        case Extract(i, 0, v: LocalVar) if size(v).isDefined && varHighZeroBits.contains(v) => {
          ChangeTo(LocalVar(v.name, BitVecType(size(v).get - varHighZeroBits(v))))
        }
        case _ => DoChildren()
      }
    }
    override def vstmt(s: Statement) = {
      s match {
        case a @ LocalAssign(lhs: LocalVar, ZeroExtend(sz, rhs), _)
            if size(lhs).isDefined && varHighZeroBits.contains(lhs) => {
          assert(varHighZeroBits(lhs) == sz)
          a.lhs = LocalVar(lhs.name, BitVecType(size(lhs).get - varHighZeroBits(lhs)))
          a.rhs = rhs
          DoChildren()
        }
        case _ => DoChildren()
      }
    }
  }
  visit_proc(ReplaceAlwaysSlicedVars(toSmallen), p)
}

def getRedundantAssignments(procedure: Procedure): Set[Assign] = {

  /** Get all assign statements which define a variable never used, assuming ssa form and proc parameters so that
    * interprocedural check is not required.
    */

  enum VS:
    case Bot
    case Assigned(definition: Set[Assign])
    case Read(definition: Set[Assign], uses: Set[CFGPosition])

  def joinVS(a: VS, b: VS) = {
    (a, b) match {
      case (VS.Bot, o)                        => o
      case (o, VS.Bot)                        => o
      case (VS.Read(d, u), VS.Read(d1, u1))   => VS.Read(d ++ d1, u ++ u1)
      case (VS.Assigned(d), VS.Read(d1, u1))  => VS.Read(d ++ d1, u1)
      case (VS.Read(d1, u1), VS.Assigned(d))  => VS.Read(d ++ d1, u1)
      case (VS.Assigned(d1), VS.Assigned(d2)) => VS.Assigned(d1 ++ d2)
    }
  }

  val assignedNotRead = mutable.Map[Variable, VS]().withDefaultValue(VS.Bot)

  for (c <- procedure) {
    c match {
      case a: LocalAssign => {
        assignedNotRead(a.lhs) = joinVS(assignedNotRead(a.lhs), VS.Assigned(Set(a)))
        a.rhs.variables.foreach(v => {
          assignedNotRead(v) = joinVS(assignedNotRead(v), VS.Read(Set(), Set(a)))
        })
      }
      case a: MemoryLoad => {
        assignedNotRead(a.lhs) = joinVS(assignedNotRead(a.lhs), VS.Assigned(Set(a)))
        a.index.variables.foreach(v => {
          assignedNotRead(v) = joinVS(assignedNotRead(v), VS.Read(Set(), Set(a)))
        })
      }
      case m: MemoryStore => {
        m.index.variables.foreach(v => {
          assignedNotRead(v) = joinVS(assignedNotRead(v), VS.Read(Set(), Set(m)))
        })
        m.value.variables.foreach(v => {
          assignedNotRead(v) = joinVS(assignedNotRead(v), VS.Read(Set(), Set(m)))
        })
      }
      case m: IndirectCall => {
        assignedNotRead(m.target) = joinVS(assignedNotRead(m.target), VS.Read(Set(), Set(m)))
      }
      case m: Assert => {
        m.body.variables.foreach(v => {
          assignedNotRead(v) = joinVS(assignedNotRead(v), VS.Read(Set(), Set(m)))
        })
      }
      case m: Assume => {
        for (v <- m.body.variables) {
          assignedNotRead(v) = joinVS(assignedNotRead(v), VS.Read(Set(), Set(m)))
        }
      }
      case c: DirectCall => {
        c.actualParams
          .flatMap(_._2.variables)
          .foreach(v => {
            assignedNotRead(v) = joinVS(assignedNotRead(v), VS.Read(Set(), Set(c)))
          })
      }
      case p: Return => {
        p.outParams
          .flatMap(_._2.variables)
          .foreach(v => {
            assignedNotRead(v) = joinVS(assignedNotRead(v), VS.Read(Set(), Set(p)))
          })
      }
      case p: GoTo        => ()
      case p: NOP         => ()
      case p: Unreachable => ()
      case p: Procedure   => ()
      case b: Block       => ()
    }
  }

  var toRemove = assignedNotRead
  var removeOld = toRemove

  val r = toRemove
    .collect { case (v, VS.Assigned(d)) =>
      d
    }
    .toSet
    .flatten
  r
}

class CleanupAssignments() extends CILVisitor {
  var redundantAssignments = Set[Assign]()

  def isRedundant(a: LocalAssign) = {
    a.lhs == a.rhs || redundantAssignments.contains(a)
  }

  override def vproc(p: Procedure) = {
    redundantAssignments = getRedundantAssignments(p)
    DoChildren()
  }

  override def vstmt(s: Statement) = s match {
    case a: LocalAssign if isRedundant(a) => ChangeTo(List())
    case _                                => SkipChildren()
  }

}


def copypropTransform(p: Procedure, procFrames: Map[Procedure, Set[Memory]], funcEntries: Map[BigInt, Procedure], constRead: (BigInt, Int) => Option[BitVecLiteral]) = {
  val t = util.PerformanceTimer(s"simplify ${p.name} (${p.blocks.size} blocks)")
  // val dom = ConstCopyProp()
  // val solver = worklistSolver(dom)

  // SimplifyLogger.info(s"${p.name} ExprComplexity ${ExprComplexity()(p)}")
  // val result = solver.solveProc(p, true).withDefaultValue(dom.bot)
  val result = CopyProp.DSACopyProp(p, procFrames, funcEntries, constRead)
  val solve = t.checkPoint("Solve CopyProp")

  if (result.nonEmpty) {
    val r = CopyProp.toResult(result, true)
    val vis = Simplify(CopyProp.toResult(result, true))
    visit_proc(vis, p)

    val condResult = CopyProp.PropFlagCalculations(p, result.toMap)
    val condVis = Simplify(CopyProp.toResult(condResult, false))
    visit_proc(condVis, p)

  }
  visit_proc(CopyProp.BlockyProp(), p)

  val xf = t.checkPoint("transform")
  // SimplifyLogger.info(s"    ${p.name} after transform expr complexity ${ExprComplexity()(p)}")

  visit_proc(CleanupAssignments(), p)
  t.checkPoint("redundant assignments")
  // SimplifyLogger.info(s"    ${p.name} after dead var cleanup expr complexity ${ExprComplexity()(p)}")

  AlgebraicSimplifications(p)
  AssumeConditionSimplifications(p)

  AlgebraicSimplifications(p)
  // SimplifyLogger.info(s"    ${p.name}  after simp expr complexity ${ExprComplexity()(p)}")
  val sipm = t.checkPoint("algebraic simp")

  // SimplifyLogger.info("[!] Simplify :: RemoveSlices")
  removeSlices(p)
  ir.eval.cleanupSimplify(p)
  AlgebraicSimplifications(p)

}

def removeEmptyBlocks(p: Program) = {
  for (proc <- p.procedures) {
    val blocks = proc.blocks.toList
    for (b <- blocks) {
      b match {
        case b: Block if b.statements.size == 0 && b.prevBlocks.size == 1 && b.jump.isInstanceOf[GoTo] => {
          val prev = b.prevBlocks
          val next = b.nextBlocks
          for (p <- prev) {
            p.jump match {
              case g: GoTo => {
                for (n <- next) {
                  g.addTarget(n)
                }
                g.removeTarget(b)
              }
              case _ => throw Exception("Must have goto")
            }
          }
          b.replaceJump(Unreachable())
          b.parent.removeBlocks(b)
        }
        case _ => ()
      }
    }
  }
}

def coalesceBlocks(p: Program) = {
  var didAny = false
  for (proc <- p.procedures) {
    val blocks = proc.blocks.toList
    for (b <- blocks.sortBy(_.rpoOrder)) {
      if (
        b.prevBlocks.size == 1 && b.prevBlocks.head.statements.nonEmpty && b.statements.nonEmpty
        && b.prevBlocks.head.nextBlocks.size == 1
        && b.prevBlocks.head.statements.lastOption.map(s => !(s.isInstanceOf[Call])).getOrElse(true)
        && !(b.parent.entryBlock.contains(b) || b.parent.returnBlock.contains(b))
      ) {
        didAny = true
        // append topredecessor
        // we know prevBlock is only jumping to b and has no call at the end
        val prevBlock = b.prevBlocks.head
        val stmts = b.statements.map(b.statements.remove).toList
        prevBlock.statements.appendAll(stmts)
        // leave empty block b and cleanup with removeEmptyBlocks
      } else if (
        b.nextBlocks.size == 1 && b.nextBlocks.head.statements.nonEmpty && b.statements.nonEmpty
        && b.nextBlocks.head.prevBlocks.size == 1
        && b.statements.lastOption.map(s => !(s.isInstanceOf[Call])).getOrElse(true)
        && !(b.parent.entryBlock.contains(b) || b.parent.returnBlock.contains(b))
      ) {
        didAny = true
        // append to successor
        // we know b is only jumping to nextBlock and does not end in a call
        val nextBlock = b.nextBlocks.head
        val stmts = b.statements.map(b.statements.remove).toList
        nextBlock.statements.prependAll(stmts)
        // leave empty block b and cleanup with removeEmptyBlocks
      }
    }
  }
  didAny
}

def removeDeadInParams(p: Program) : Boolean = {
  var modified = false
  assert(invariant.correctCalls(p))

  for (block <- p.procedures.flatMap(_.entryBlock)) {
    val proc = block.parent

    val (liveBefore,_) = getLiveVars(proc)
    val live = liveBefore(block)
    val unused = proc.formalInParam.filterNot(live.contains(_))

    for (unusedFormalInParam <- unused) {
      modified = true
      proc.formalInParam.remove(unusedFormalInParam)

      for (call <- proc.incomingCalls()) {
        call.actualParams = call.actualParams.removed(unusedFormalInParam)
      }
    }
  }

  if (modified) assert(invariant.correctCalls(p))
  modified
}

/*
 * Inline procedure output parameters where the procedure returns a constant or the same out param as the in param.
 */
def removeInvariantOutParameters(p: Program): Boolean = {
  assert(invariant.correctCalls(p))
  assert(invariant.singleCallBlockEnd(p))
  var modified = false

  val returns = p.procedures.flatMap(_.returnBlock).map(_.jump).collect { case r: Return => r }
  for (ret <- returns) {
    val proc = ret.parent.parent
    val inParams = proc.formalInParam.toSet

    val invariantParams = ret.outParams.collect {
      // we are returning a constant and can inline
      case (formalOut, binding: Literal) => (formalOut, binding)
      // we are returning the input parameter with the same name as the output parameter so can inline at callsite
      case (formalOut, binding: LocalVar) if inParams.contains(binding) => (formalOut, binding)
    }

    // remove invariant params from outparam signature, and outparam list of return, and out param list of all calls,
    // and add assignment after the call of bound actual param to bound outparam
    for ((invariantOutFormal, binding) <- invariantParams) {
      modified = true
      proc.formalOutParam.remove(invariantOutFormal)
      ret.outParams = ret.outParams.removed(invariantOutFormal)

      val calls = proc.incomingCalls()

      for (call <- calls) {
        val lhs = call.outParams(invariantOutFormal)
        val rhs = binding 
        call.outParams = call.outParams.removed(invariantOutFormal)

        // insert assignment of to successor to maintain singleCallBlockEnd invariant
        call.parent.jump match {
          case r : Return => {
            // substitute directly into return
            r.outParams = r.outParams.map((f, a) => (f, Substitute(v => if (v == lhs) then Some(rhs) else None)(a).getOrElse(a)))
          }
          case r: Unreachable => ()
          case g: GoTo => {
            // add assignment
            for (t <- g.targets) {
              t.statements.prepend(LocalAssign(lhs, rhs))
            }
          }
        }

        call.parent.statements.insertBefore(call, LocalAssign(lhs, rhs))
      }
    }
  }

  if (modified) {
    assert(invariant.correctCalls(p))
    assert(invariant.singleCallBlockEnd(p))
  }
  modified
}

def doCopyPropTransform(p: Program, rela: Map[BigInt, BigInt]) = {

  applyRPO(p)

  def isExternal(p: Procedure) = p.isExternal.contains(true) || p.blocks.isEmpty
  // assume some functions modify nothing 
  def noModifies(p: Procedure) = 
    p.procName match {
    case "strlen" | "assert" | "printf"  | "__stack_chk_fail" | "__printf_chk" | "__syslog_chk" => true
    case _ => false
  }

  val procFrames = p.procedures.filterNot(p => (isExternal(p) && !(noModifies(p))))
    .map(p => (p, getProcFrame.apply(p))).toMap

  val addrToProc = p.procedures.toSeq.flatMap(p => p.address.map(addr => addr -> p).toSeq).toMap

  def read(addr: BigInt, size: Int) : Option[BitVecLiteral] = {
    val rodata = p.initialMemory.filter((_, s) => s.readOnly)
    rodata.maxBefore(addr + 1) match {
      case None => None
      case (Some((k, s))) if s.canGetBytes(addr, size / 8) => {
        SimplifyLogger.debug(s"got [$addr..${addr + size}] from ${s.name} [${s.address}..${s.address + s.size}]")
        Some(s.getBytes(addr, size / 8).reverse.foldLeft(BitVecLiteral(0,0))((acc, r) => ir.eval.BitVectorEval.smt_concat(acc, r)))
      }
      case (Some((k,s))) => {
        SimplifyLogger.debug(s"Cannot get [$addr..${addr + size}] from ${s.name} [${s.address}..${s.address + s.size}]")
        None
      }
    }
  }

  SimplifyLogger.info("[!] Simplify :: Expr/Copy-prop Transform")
  val work = p.procedures
    .filter(_.blocks.size > 0)
    .map(p =>
      p -> //Future
        {
          SimplifyLogger
            .debug(s"CopyProp Transform ${p.name} (${p.blocks.size} blocks, expr complexity ${ExprComplexity()(p)})")
          copypropTransform(p, procFrames, addrToProc, read)
        }
    )

  work.foreach((p, job) => {
    try {
      //Await.result(job, 10000.millis)
      job
    } catch {
      case e => {
        SimplifyLogger.error("Simplify :: CopyProp " + p.name + ": " + e.toString)
      }
    }
  })

  SimplifyLogger.info("[!] Simplify :: Dead variable elimination")

  // cleanup
   visit_prog(CleanupAssignments(), p)
  SimplifyLogger.info("[!] Simplify :: Merge empty blocks")

  removeEmptyBlocks(p)
  coalesceBlocks(p)
  removeEmptyBlocks(p)
  coalesceBlocks(p)
  removeEmptyBlocks(p)
  coalesceBlocks(p)
  removeEmptyBlocks(p)
  coalesceBlocks(p)
  removeEmptyBlocks(p)
  visit_prog(CopyProp.BlockyProp(), p)
  visit_prog(CleanupAssignments(), p)

}

def copyPropParamFixedPoint(p: Program, rela: Map[BigInt, BigInt]): Int = {
  doCopyPropTransform(p, rela)
  var changed = removeInvariantOutParameters(p)
  var iterations = 1
  if (changed) {
    SimplifyLogger.info(s"Simplify:: Copyprop iteration $iterations")
    doCopyPropTransform(p, rela)
    changed = removeInvariantOutParameters(p)
    changed = changed | removeDeadInParams(p)
    iterations += 1
  }
  iterations
}

def reversePostOrder(p: Procedure): Unit = {
  /* Procedures may contain disconnected sets of blocks so we arbitrarily order these with respect to eachother. */
  for (b <- p.blocks) {
    b.rpoOrder = -1
  }
  var left = p.entryBlock.map(reversePostOrder(_)).getOrElse(0) + 1
  for (b <- p.blocks.filter(_.rpoOrder == -1)) {
    left = reversePostOrder(b, true, left) + 1
  }
}

def reversePostOrder(startBlock: Block, fixup: Boolean = false, begin: Int = 0): Int = {
  var count = begin
  val seen = mutable.HashSet[Block]()
  val vcs = mutable.HashMap[Block, Int]()

  def walk(b: Block): Unit = {
    seen += b
    for (s <- b.nextBlocks) {
      if (!seen.contains(s)) {
        walk(s)
      }
    }
    if (!fixup || b.rpoOrder < count) {
      b.rpoOrder = count
    }
    count += 1
  }

  walk(startBlock)
  count
}

def applyRPO(p: Program) = {
  for (proc <- p.procedures) {
    reversePostOrder(proc)
  }
}

// case class CopyProp(from: Expr, expr: Expr, deps: Set[Variable])

enum CopyProp {
  case Bot
  case Prop(expr: Expr, deps: Set[Variable])
  case Clobbered
}

case class CCP(val state: Map[Variable, CopyProp] = Map())


object getProcFrame {
  class GetProcFrame extends CILVisitor  {
    var modifies = Set[Memory]()

    override def vstmt(e: Statement) = e match {
      case s : MemoryStore => modifies = modifies + s.mem; SkipChildren()
      case _ => SkipChildren()
    }

  }

  def apply(p: Procedure): Set[Memory] = {
    val v = GetProcFrame()
    visit_proc(v, p)
    v.modifies
  }
}

object CCP {

  def toSubstitutions(c: CCP): Map[Variable, Expr] = {
    c.state.collect { case (v, CopyProp.Prop(e, _)) =>
      v -> e
    }
  }

  def clobberFull(c: CCP, l: Variable) = {
    val p = clobber(c, l)
    p.copy(state = p.state + (l -> CopyProp.Clobbered))
  }

  def clobber(c: CCP, l: Variable) = {
    CCP(
      c.state
        .map((k, v) =>
          k -> (v match {
            case CopyProp.Prop(_, deps) if deps.contains(l) => CopyProp.Clobbered
            case o                                          => o
          })
        )
        .withDefaultValue(CopyProp.Bot)
    )
  }
}

object CopyProp {

  class BlockyProp() extends CILVisitor {
    /* Flow-sensitive intra-block copyprop */

    var st = Map[Variable, Expr]()

    def subst(e: Expr) : Expr = {
      simplifyExprFixpoint(Substitute(
        v =>  st.get(v).filter(isTrivial),
        true
      )(e).getOrElse(e))(0)
    }

    override def vblock(b: Block) = {
      st = Map()
      DoChildren()
    }

    override def vstmt(s: Statement) = {
      s match {
        case l : LocalAssign => {
          val nrhs = subst(l.rhs)
          st = st.removedAll(st.keys)
          st = st.updated(l.lhs, nrhs)
          l.rhs = nrhs 
          SkipChildren()
        }
        case x : Assert => {
          x.body = subst(x.body)
          SkipChildren()
        }
        case x : Assume => {
          x.body = subst(x.body)
          SkipChildren()
        }
        case x: DirectCall => {
          x.actualParams = x.actualParams.map((l,r) => (l, subst(r)))
          val lhs = x.outParams.map(_._2)
          st = st.removedAll(lhs)
          SkipChildren()
        }
        case d : MemoryLoad => {
          d.index = subst(d.index)
          st = st.removed(d.lhs)
          SkipChildren()
        }
        case d : MemoryStore => {
          d.index = subst(d.index)
          d.value = subst(d.value)
          SkipChildren()
        } 
        case x: IndirectCall => {
          st = Map()
          SkipChildren()
        }
        case _ : NOP => SkipChildren()
      }
    }
  }


  def isFlagVar(l: Variable) = {
    val flagNames = Set("ZF", "VF", "CF", "NF")
    l match {
      case l: LocalVar => flagNames.contains(l.varName)
      case l           => flagNames.contains(l.name)
    }
  }

  case class PropState(
    var e: Expr,
    val deps: mutable.Set[Variable],
    var clobbered: Boolean,
    var useCount: Int,
    var isFlagDep: Boolean
  )

  def PropFlagCalculations(p: Procedure, initialState: Map[Variable, PropState]) = {
    val state = mutable.HashMap[Variable, PropState]()

    val flagDeps = initialState.collect {
      case (v, e) if e.isFlagDep => v -> e
    }

    val worklist = mutable.PriorityQueue[Block]()(Ordering.by(_.rpoOrder))
    worklist.addAll(p.blocks)
    var poisoned = false

    def transfer(s: Statement) = {
      s match {
        case a: MemoryLoad => clobberFull(state, a.lhs)
        case a: LocalAssign if !state.contains(a.lhs) && flagDeps.contains(a.lhs) => {
          val (r,deps) = canPropTo(state, a.rhs, true).get
          state(a.lhs) = PropState(r, mutable.Set.from(r.variables), false, 0, true)
        }
        case a: LocalAssign if state.contains(a.lhs) && state(a.lhs).clobbered => {
          ()
        }
        case a: LocalAssign
            if state.contains(a.lhs) && (a.rhs != state(a.lhs).e) && (canPropTo(state, a.rhs, true) != canPropTo(
              state,
              state(a.lhs).e,
              true
            )) => {
          clobberFull(state, a.lhs)
        }
        case a: LocalAssign if state.contains(a.lhs) => {
          val (ne, deps) = canPropTo(state, a.rhs, true).get
          state(a.lhs).e = ne
          state(a.lhs).deps.addAll(deps)
        }
        case i: IndirectCall => {
          poisoned = true
        }
        case d: DirectCall => {
          for (l <- d.outParams.map(_._2)) {
            clobberFull(state, l)
          }
        }
        case _ => ()
      }
    }

    while (worklist.nonEmpty && !poisoned) {
      val b: Block = worklist.dequeue

      for (l <- b.statements) {
        transfer(l)
      }
    }
    if (poisoned) then mutable.HashMap() else 
    state
  }

  def clobberFull(c: mutable.HashMap[Variable, PropState], l: Variable): Unit = {
    clobberOne(c, l)
    for (v <- c.keys) {
      if (c(v).deps.contains(l)) {
        clobberOne(c, v)
      }
    }
  }

  def clobberOne(c: mutable.HashMap[Variable, PropState], l: Variable): Unit = {
    if (c.contains(l) && !c(l).clobbered) {
      c(l).clobbered = true
    } else if (!c.contains(l)) {
      c(l) = PropState(FalseLiteral, mutable.Set(), true, 0, false)
    }
  }

  def isTrivial(e: Expr): Boolean = e match {
    case l: Literal                              => true
    case l: Variable                             => true
    case BinaryExpr(op, e: Variable, b: Literal) => true
    case ZeroExtend(_, e) if isTrivial(e)        => true
    case SignExtend(_, e) if isTrivial(e)        => true
    case Extract(_, _, e) if isTrivial(e)        => true
    case UnaryExpr(_, e) if isTrivial(e)         => true
    case _                                       => false
  }

  def canPropTo(s: mutable.HashMap[Variable, PropState], e: Expr, isFlag: Boolean = false): Option[(Expr, Set[Variable])] = {

    def proped(e: Expr) = {
      var deps = Set[Variable]() ++ e.variables
      val ne =
        if e.variables.size > 1 && !isFlag then Some(e)
        else
          Substitute(
            v => {
              s.get(v) match {
                case Some(vs) if !vs.clobbered => {
                  deps = deps ++ vs.deps + v
                  Some(vs.e)
                }
                case _                         => None
              }
            },
            true
          )(e)

      // partial eval after prop
      (simplifyExprFixpoint(ne.getOrElse(e))._1, deps)
    }


    val (p, deps) = proped(e)
    p match {
      case l: Literal                                 => Some(l, deps)
      case l: Variable                                => Some(l, deps)
      case e @ BinaryExpr(o, v: Variable, c: Literal) => Some(e, deps)
      case e: Expr if isFlag || e.variables.size <= 1 => Some(e, deps)
      case e                                          => None
    }
  }

  def varForMem(m: Memory) = Register("symbolic_memory_var" + m.name, 1)
  def isMemVar(v: Variable) = v.name.startsWith("symbolic_memory_var")
  def varForLoadStore(m: Memory, addr: Expr , sz: Int) = {
    Register(m.name + "_symbolic_store_" + translating.PrettyPrinter.pp_expr(addr), sz)
  }

  def clobberAllMemory(c: mutable.HashMap[Variable, PropState]) = {
    c.keys.filter(isMemVar).foreach(v => clobberFull(c, v))
  }

  def DSACopyProp(p: Procedure, procFrames: Map[Procedure, Set[Memory]], funcEntries: Map[BigInt, Procedure], constRead: (BigInt, Int) => Option[BitVecLiteral]) = {
    val updated = false
    val state = mutable.HashMap[Variable, PropState]()
    var poisoned = false // we have an indirect call

    val doLoadReasoning = false

    def transfer(c: mutable.HashMap[Variable, PropState], s: Statement): Unit = {
      // val callClobbers = ((0 to 7) ++ (19 to 30)).map("R" + _).map(c => Register(c, 64))
      s match {
        case l : MemoryStore if doLoadReasoning => {
          val mvar = varForMem(l.mem)
          val existing = c.get(mvar).isDefined

          if (existing) {
            clobberFull(c, varForMem(l.mem))
          } else {
            c(mvar) = PropState(FalseLiteral, mutable.Set.empty, false, 0, false)

            val store = canPropTo(c, l.index) 
            
            store match {
              case (Some((addr),deps)) => {
                val storeVar = varForLoadStore(l.mem, addr, l.size)
                if (c.get(storeVar).isDefined) {
                  clobberFull(c, storeVar)
                } else {
                  val value = canPropTo(c, l.value) 
                  value match {
                    case Some(value, deps) => {
                      c(storeVar) = PropState(value, mutable.Set.from(deps + mvar), false, 0, false)
                    }
                    case _ =>  clobberFull(c, storeVar)
                  }
                }
              }
              case _ => ()
            }
          }
        }
        case l: MemoryLoad if doLoadReasoning => {
          val loadprop = canPropTo(c, l.index)
          val loaded = for {
            (addr, deps) <-  loadprop
            loadvar = varForLoadStore(l.mem, addr, l.size)
            loadval = canPropTo(c, loadvar).filterNot((v, _) => v == loadvar)
            (value, ndeps) <- loadval.orElse(addr match {
              case b : BitVecLiteral => {
                val r = constRead(b.value, l.size)
                r.map(v => (v, Set[Variable]()))
              }
              case r => {
                None
              }
            })
          } yield (value, deps ++ ndeps)
          loaded match {
            case (Some(value, deps)) => 
              c(l.lhs) = PropState(value, mutable.Set.from(deps), false, 0, false)
          case _ => {
            clobberFull(c, l.lhs)
          }
          }
        }
        case l : MemoryStore  => {
          ()
        }
        case l: MemoryLoad => {
            clobberFull(c, l.lhs)
        }
        case LocalAssign(l, r, lb) => {
          val isFlag = isFlagVar(l) || r.variables.exists(isFlagVar)
          val isFlagDep = isFlag || c.get(l).map(v => v.isFlagDep).getOrElse(false)

          c.get(l).foreach(v => v.isFlagDep = v.isFlagDep || isFlagDep)
          for (l <- r.variables) {
            c.get(l).foreach(v => v.isFlagDep = v.isFlagDep || isFlagDep)
          }

          var prop = canPropTo(c, r, isFlagDep)
          val existing = c.get(l)

          (prop, existing) match {
            case (Some(evaled, deps), None) => {
              c(l) = PropState(evaled, mutable.Set.from(deps), false, 0, isFlagDep)
            }
            case (_, Some(ps)) if ps.clobbered => {
              ()
            }
            case (Some(evaled, deps), Some(ps))  if ps.e == r || ps.e == evaled || (canPropTo(c, ps.e, isFlagDep).contains(evaled)) => {
                c(l).e = evaled
                c(l).deps.addAll(deps)
            }
            case _ => {
              // ps.e != evaled and have prop
              clobberFull(c, l)
            }
          }
        }
        case x: DirectCall => {
          procFrames.get(x.target) match {
            case Some(f) => for (mem <- f) {
              clobberFull(c, varForMem(mem))
            }
            case _ => clobberAllMemory(c)
          }

          val lhs = x.outParams.map(_._2)
          for (l <- lhs) {
            clobberFull(c, l)
          }
        }
        case x: IndirectCall => {
          // need a reaching-defs to get inout args (just assume register name matches?)
          // this reduce we have to clobber with the indirect call this round
          if (!doLoadReasoning) {
              poisoned = true
          }
          val r = for {
            (addr, deps) <- canPropTo(c, x.target)
            addr <- addr match {
              case b : BitVecLiteral => Some(b.value)
              case _ => None
            }
            proc <- funcEntries.get(addr)
          } yield (proc, deps)

          r match {
            case Some(target, deps) => {
              SimplifyLogger.info("Resolved indirect call")
            }
            case None => {
              for ((i, v) <- c) {
                v.clobbered = true
              }
              poisoned = true
            }
          }

        }
        case _ => ()
      }
    }

    // sort by precedence
    val worklist = mutable.PriorityQueue[Block]()(Ordering.by(_.rpoOrder))
    worklist.addAll(p.blocks)

    while (worklist.nonEmpty && !poisoned) {
      val b: Block = worklist.dequeue
      for (l <- b.statements) {
        transfer(state, l)
      }
    }

    val trivialOnly = false

    if (!poisoned) state else mutable.HashMap()
  }

  def toResult(s: mutable.Map[Variable, PropState], trivialOnly: Boolean = true)(v: Variable): Option[Expr] = {
    s.get(v) match {
      case Some(c) if !c.clobbered && (!trivialOnly || isTrivial(c.e))  => Some(c.e)
      case _                                                           => None
    }
  }

}

class ExprComplexity extends CILVisitor {
  // count the nodes in the expression AST
  var count = 0
  override def vexpr(e: Expr) = {
    count += 1
    DoChildren()
  }

  def apply(e: Procedure) = {
    count = 0
    visit_proc(this, e)
    count
  }

  def apply(e: Expr) = {
    count = 0
    visit_expr(this, e)
    count
  }
}

/** Use this as a partially applied function. Substitute(Map.from(substs).get, recurse = false)
  *
  * @res:
  *   defines the substitutions to make
  * @recurse:
  *   continue substituting with `res` into each substituted expression
  * @complexityThreshold:
  *   Stop substituting after the AST node count has increased by this much
  */
class Substitute(val res: Variable => Option[Expr], val recurse: Boolean = true, val complexityThreshold: Int = 0)
    extends CILVisitor {
  var madeAnyChange = false
  var complexity = 0

  override def vexpr(e: Expr) = {
    e match {
      case v: Variable if res(v).isDefined => {
        val changeTo = res(v).get
        if (complexityThreshold > 0) {
          complexity += ExprComplexity()(changeTo) - ExprComplexity()(e)
        }
        if (complexityThreshold > 0 && complexity > complexityThreshold) {
          SkipChildren()
        } else if (recurse) {
          madeAnyChange = true
          ChangeDoChildrenPost(changeTo, x => x)
        } else {
          madeAnyChange = true
          ChangeTo(changeTo)
        }
      }
      case e => DoChildren()
    }
  }

  def apply(e: Expr): Option[Expr] = {
    madeAnyChange = false
    val ne = visit_expr(this, e)
    val changed = madeAnyChange
    madeAnyChange = false
    if (changed) {
      Some(ne)
    } else {
      None
    }
  }
}

enum PathExit:
  case Maybe
  case Return
  case NoReturn
  case Bot

object PathExit:
  def join(l: PathExit, r: PathExit) = (l, r) match {
    case (PathExit.Return, PathExit.Return)     => PathExit.Return
    case (PathExit.NoReturn, PathExit.NoReturn) => PathExit.NoReturn
    case (PathExit.Return, PathExit.NoReturn)   => PathExit.Maybe
    case (PathExit.NoReturn, PathExit.Return)   => PathExit.Maybe
    case (PathExit.Maybe, x)                    => PathExit.Maybe
    case (x, PathExit.Maybe)                    => PathExit.Maybe
    case (PathExit.Bot, x)                      => x
    case (x, PathExit.Bot)                      => x
  }

class ProcExitsDomain(is_nonreturning: String => Boolean) extends AbstractDomain[PathExit] {
  /* backwards analysis to identify non-returning function */

  override def transfer(s: PathExit, b: Command) = {
    b match {
      case d: DirectCall if is_nonreturning(d.target.name) => PathExit.NoReturn
      case r: Return                                       => PathExit.Maybe
      case _                                               => s
    }
  }
  override def top = PathExit.Maybe
  def bot = PathExit.Bot
  def join(l: PathExit, r: PathExit, pos: Block) = PathExit.join(l,r)
}

case class ProcReturnInfo(returning: Set[Procedure], nonreturning: Set[Procedure]) {
  override def toString = s"returning : ${returning.map(_.name).toList.sorted}\nnonretruning: ${nonreturning.map(_.name).toList.sorted}"
}

class DefinitelyExits(knownExit: Set[Procedure]) extends ProcedureSummaryGenerator[PathExit, PathExit] {
  def top: ir.transforms.PathExit = ???
  def bot: ir.transforms.PathExit = PathExit.Bot
  override def init (p: Procedure) = if p.procName == "exit" then PathExit.NoReturn else PathExit.Bot
  def join(l: PathExit, r: PathExit, p: Procedure) = PathExit.join(l,r)

  def transfer
  (a: ir.transforms.PathExit, b: ir.Procedure):
    ir.transforms.PathExit = ???
  
  def localTransferCall
  (l: ir.transforms.PathExit, summaryForTarget: ir.transforms.PathExit, p: ir.DirectCall)
    : ir.transforms.PathExit = (l, summaryForTarget) match {
      case (PathExit.Return, PathExit.Return)  => PathExit.Return
      case (_, PathExit.NoReturn) => PathExit.NoReturn
      case (o, _)  => o
  }

  def updateSummary
  (prevSummary: ir.transforms.PathExit, p: ir.Procedure,
    resBefore: Map[ir.Block, ir.transforms.PathExit], resAfter:
    Map[ir.Block, ir.transforms.PathExit]): ir.transforms.PathExit = {
      p.entryBlock.flatMap(resBefore.get) match {
        case Some(PathExit.NoReturn) => PathExit.NoReturn
        case Some(PathExit.Return)   => PathExit.Return
        case Some(PathExit.Maybe)    => PathExit.Maybe
        case  _                      => prevSummary
    }
  }
}

def findDefinitelyExits(p: Program) = {
  val exit = p.procedures.filter(p => p.procName == "exit").toSet
  val dom = DefinitelyExits(exit)
  val ldom = ProcExitsDomain(x => false)
  val solve = interprocSummaryFixpointSolver(ldom, dom)
  val res = solve.solveProgInterProc(p, true)
  ProcReturnInfo(res.collect {
    case (p, PathExit.Return) => p
  }.toSet, 
  res.collect {
    case (p, PathExit.NoReturn) => p
  }.toSet
  )

}

class Simplify(val res: Variable => Option[Expr], val initialBlock: Block = null) extends CILVisitor {

  var madeAnyChange = false
  var block: Block = initialBlock
  var skipped = Set[String]()

  override def vexpr(e: Expr) = {
    val threshold = 500
    val variables = e.variables.toSet
    val subst = Substitute(res, true, threshold)
    var old: Expr = e
    var result = subst(e).getOrElse(e)
    while (old != result) {
      old = result
      result = subst(e).getOrElse(e)
    }

    if (subst.complexity > threshold) {
      val bl = s"${block.parent.name}::${block.label}"
      if (!skipped.contains(bl)) {
        skipped = skipped + bl
        SimplifyLogger.warn(s"Some skipped substitution at $bl due to resulting expr size > ${threshold} threshold")
      }
    }
    ChangeDoChildrenPost(result, x => x)
  }

  override def vblock(b: Block) = {
    block = b
    DoChildren()
  }
}

