package ir
import scala.util.boundary, boundary.break
import java.io.File
import ir.dsl.*
import scala.collection.mutable
import analysis.Loop
import ir.dsl.IRToDSL
import ir.cilvisitor.*
import translating.PrettyPrinter.*
import ir.dsl.IRToDSL.sequenceTransitionSystems
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
import util.Logger
import ir.transforms.Substitute

val linkUninterpByAxioms = true

def boolAnd(exps: Iterable[Expr]) =
  val l = exps.toList
  l.size match {
    case 0 => TrueLiteral
    case 1 => l.head
    case _ => BoolExp(BoolAND, l)
  }

def boolOr(exps: Iterable[Expr]) =
  val l = exps.toList
  l.size match {
    case 0 => TrueLiteral
    case 1 => l.head
    case _ => BoolExp(BoolOR, l)
  }

def polyEqual(e1: Expr, e2: Expr) = {
  (e1.getType, e2.getType) match {
    case (l,r) if l == r => BinaryExpr(EQ, e1, e2)
    case (BitVecType(sz1), BitVecType(sz2)) if sz1 > sz2 => BinaryExpr(EQ, e1, ZeroExtend(sz1 - sz2, e2))
    case (BitVecType(sz1), BitVecType(sz2)) if sz1 < sz2 => BinaryExpr(EQ, ZeroExtend(sz2 - sz1, e1), e2)
    case (a, b) => throw Exception(s"wierd type $a == $b")
  }
}

class NamespaceState(val namespace: String) extends CILVisitor {

  def stripNamespace(n: String) = n.stripPrefix(namespace + "__")

  override def vblock(b: Block) = {
    b.label = namespace + "__" + b.label
    DoChildren()
  }

  override def vexpr(e: Expr) = e match {
    case f @ UninterpretedFunction(n, p, r, _) if linkUninterpByAxioms =>
      ChangeDoChildrenPost(f.copy(name = namespace + "__" + f.name), x => x)
    // case f @ UninterpretedFunction(n, p, r, _) if n.startsWith("load") =>
    //  ChangeDoChildrenPost(f.copy(name = namespace + "__" + f.name), x => x)
    // case f @ UninterpretedFunction(n, p, r, _) if n.startsWith("trace_load") =>
    //  ChangeDoChildrenPost(f.copy(name = namespace + "__" + f.name), x => x)
    // case f @ UninterpretedFunction(n, p, r, _) if n.startsWith("store") =>
    //  ChangeDoChildrenPost(f.copy(name = namespace + "__" + f.name), x => x)
    case _ => DoChildren()
  }

  override def vlvar(v: Variable) = v match {
    case l: LocalVar => ChangeTo(l.copy(varName = namespace + "__" + l.varName))
    case l: GlobalVar => ChangeTo(l.copy(name = namespace + "__" + l.name))
  }
  override def vrvar(v: Variable) = v match {
    case l: LocalVar => ChangeTo(l.copy(varName = namespace + "__" + l.varName))
    case l: GlobalVar => ChangeTo(l.copy(name = namespace + "__" + l.name))
  }

  override def vmem(m: Memory) = m match {
    case m: SharedMemory => ChangeTo(m.copy(name = namespace + "__" + m.name))
    case m: StackMemory => ChangeTo(m.copy(name = namespace + "__" + m.name))
  }
}

val traceType = CustomSort("Tr")
val transitionSystemPCVar = GlobalVar("SYNTH_PC", IntType)

val traceVar = {
  GlobalVar("TRACE", traceType)
}

object AssertsToAssumes extends CILVisitor {
  override def vstmt(s: Statement) = s match {
    case Assert(a, b, c) => ChangeTo(List(Assume(a, b, Some("(passified assert) " + c.getOrElse("")))))
    case _ => SkipChildren()
  }
}

object Ackermann {

  case class Info(
    call: String,
    prefix: String,
    argAssign: SimulAssign,
    returnAssign: SimulAssign,
    // to maintain arg order to correspond with target
    args: List[Variable],
    returns: List[Variable]
  )

  class Passify extends CILVisitor {
    override def vstmt(s: Statement) = s match {
      case SimulAssign(assignments, _) => {
        ChangeTo(List(Assume(boolAnd(assignments.map(polyEqual)))))
      }
      case _ => SkipChildren()
    }
  }

  def passify(p: Procedure) = {
    visit_proc(Passify(), p)
  }

  class ToAssume(axioms: Map[SimulAssign, Info]) extends CILVisitor {
    override def vstmt(s: Statement) = s match {
      case s: SimulAssign if axioms.contains(s) => {
        ChangeTo(s.assignments.map(polyEqual).map(x => Assume(x, Some(axioms(s).call))).toList)
      }
      case _ => SkipChildren()
    }
  }

  class AckermannTransform(stripNamespace: String => String, traceVars: Set[Variable]) extends CILVisitor {
    // expected to run on the program with side effects removed,
    // prior to namespacing
    //

    var axioms = Map[SimulAssign, Info]()

    var counter = 0

    def isTraceVar(v: Variable) = {
      stripNamespace(v.name).startsWith("TRACE")
    }

    override def vstmt(s: Statement) = s match {
      case s: SimulAssign if s.assignees.exists(isTraceVar) => {
        counter += 1

        val rg = s.assignments.find(v => isTraceVar(v._1)).get

        val (fname, args) = rg._2 match {
          case UninterpretedFunction(name, args, rt, _) => (name, args)
          case _ => ???
        }

        val newargs = args.zipWithIndex.map { case (rhs, index) =>
          LocalVar(s"${fname}_ackarg${counter}_${index}", rhs.getType) -> rhs
        }
        val newreturns = s.assignments.zipWithIndex.map { case ((lhs, rhs), index) =>
          lhs -> LocalVar(s"${fname}_ackret_${counter}_${index}", lhs.getType)
        }

        val (argAssign, returnAssign) = (SimulAssign(newargs.toVector), SimulAssign(newreturns.toVector))

        val argvars = newargs.map(_._1).toList
        val returnvars = newreturns.map(_._2).toList

        val prefix = fname.stripSuffix(stripNamespace(fname))

        axioms =
          axioms.updated(argAssign, Info(stripNamespace(fname), prefix, argAssign, returnAssign, argvars, returnvars))

        ChangeTo(List(argAssign, returnAssign))
      }
      case _ => SkipChildren()
    }
  }

  def doTransform(p: Procedure, srcNamespace: NamespaceState, tgtNamespace: NamespaceState): Map[SimulAssign, Info] = {
    def stripNamespace(s: String) = {
      tgtNamespace.stripNamespace(srcNamespace.stripNamespace(s))
    }

    val v =
      AckermannTransform(stripNamespace, Set(visit_rvar(srcNamespace, traceVar), visit_rvar(tgtNamespace, traceVar)))
    visit_proc(v, p)
    v.axioms
  }

  def getVisitor(srcNamespace: NamespaceState, tgtNamespace: NamespaceState) = {
    def stripNamespace(s: String) = {
      tgtNamespace.stripNamespace(srcNamespace.stripNamespace(s))
    }

    AckermannTransform(stripNamespace, Set(visit_rvar(srcNamespace, traceVar), visit_rvar(tgtNamespace, traceVar)))
  }

  def naiveInvariant(inst: Map[SimulAssign, Info]): List[Expr] = {

    val compat = inst.groupBy(_._2.call)

    compat
      .flatMap((pref, insts) => {
        val gs = inst.groupBy(_._2.prefix).toList
        val fst = gs.head._2
        val snd = gs.tail.head._2
        assert(gs.tail.tail.isEmpty)

        for {
          s <- fst
          t <- snd
          argsEqual = s._2.args.zip(t._2.args).map(polyEqual)
          returnsEqual = s._2.returns.zip(t._2.returns).map(polyEqual)
        } yield (BinaryExpr(BoolIMPLIES, boolAnd(argsEqual), boolAnd(returnsEqual)))
      })
      .toList

  }

  def instantiateAxioms(
    sourceEntry: Block,
    targetEntry: Block,
    instantiations: Map[SimulAssign, Info],
    inlineArgs: Boolean = true
  ) = {

    var seen = Set[CFGPosition]()
    var invariant = List[Expr]()

    def succ(p: CFGPosition) =
      var n = IntraProcIRCursor.succ(p)
      while (
        (n.size == 1) && (n.head match {
          case s: SimulAssign if instantiations.contains(s) => false
          case _ => true
        })
      ) {
        // skip statements within stright lines of execution
        n = IntraProcIRCursor.succ(n.head)
      }
      val r = n.filterNot(seen.contains(_)).map {
        case s: SimulAssign if instantiations.contains(s) => (Some(s), s)
        case s => (None, s)
      }
      r

    val q = mutable.Queue[((Option[SimulAssign], CFGPosition), (Option[SimulAssign], CFGPosition))]()
    val start = ((None, sourceEntry), (None, targetEntry))
    if (instantiations.nonEmpty) {
      q.enqueue(start)
    }

    while (q.nonEmpty) {
      val ((srcCall, srcPos), (tgtCall, tgtPos)) = q.dequeue

      def advanceBoth() = {
        for (s <- succ(srcPos)) {
          for (t <- succ(tgtPos)) {
            q.enqueue((s, t))
          }
        }
      }

      def advanceSrc() = {
        for (s <- succ(srcPos)) {
          q.enqueue((s, (tgtCall, tgtPos)))
        }
      }

      def advanceTgt() = {
        for (t <- succ(tgtPos)) {
          q.enqueue(((srcCall, srcPos), t))
        }
      }

      def label(s: CFGPosition) = s match {
        case p: Procedure => p.name
        case b: Block => b.label
        case s: Command => s.getClass.getSimpleName
      }
      (srcCall, tgtCall) match {
        case (None, None) => advanceBoth()
        case (None, Some(_)) => advanceSrc()
        case (Some(_), None) => advanceTgt()
        case (Some(src), Some(tgt)) => {
          seen = seen ++ Seq(src, tgt)

          def getArgs(i: Info) = if inlineArgs then {
            i.argAssign.assignments.map(_._2).toList
          } else {
            i.args.toList
          }

          val srcInfo = instantiations(src)
          val tgtInfo = instantiations(tgt)
          if (srcInfo.call == tgtInfo.call) {
            val argsEqual = getArgs(srcInfo).zip(getArgs(tgtInfo)).map(polyEqual)
            val returnsEqual = srcInfo.returns.toList.zip(tgtInfo.returns).map(polyEqual)
            invariant = BinaryExpr(BoolIMPLIES, boolAnd(argsEqual), boolAnd(returnsEqual)) :: invariant
            advanceBoth()
          }
        }
        case _ => ()

      }

    }

    class RemoveArgCopy extends CILVisitor {
      override def vstmt(s: Statement) = s match {
        case s: SimulAssign if instantiations.contains(s) => ChangeTo(List())
        case _ => SkipChildren()
      }
    }

    if (inlineArgs) {
      visit_proc(RemoveArgCopy(), sourceEntry.parent)
    }

    invariant
  }
}

class RewriteSideEffects() extends CILVisitor {

  def loadFunc(lhs: Variable, size: Int, addr: Expr) = {
    val loadValue =
      (lhs, UninterpretedFunction("load_" + size, Seq(traceVar, addr), BitVecType(size)))
    val trace =
      (traceVar, UninterpretedFunction("trace_load_" + size, Seq(traceVar, addr), traceType))
    List(SimulAssign(Vector(trace, loadValue)))

  }

  def storeFunc(size: Int, addr: Expr, value: Expr) =
    (SimulAssign(
      Vector(traceVar -> UninterpretedFunction("store_" + size, Seq(traceVar, addr, value: Expr), traceType))
    ))

  def directCallFunc(m: DirectCall) = {
    val params = traceVar :: m.actualParams.toList.map(_._2)
    val trace =
      traceVar ->
        UninterpretedFunction("Call_" + m.target.name, params, traceType)
    val outParams = m.outParams.toList
      .map(p =>
        p._2 ->
          UninterpretedFunction("Call_" + m.target.name + "_" + p._1.name, params, p._2.getType)
      )
      .toList

    List(SimulAssign((trace :: outParams).toVector))
  }

  override def vstmt(s: Statement) = s match {
    case m: MemoryLoad => ChangeTo(loadFunc(m.lhs, m.size, m.index))
    case m: MemoryStore => ChangeTo(List(storeFunc(m.size, m.index, m.value)))
    case m: IndirectCall =>
      ChangeTo(List(LocalAssign(traceVar, UninterpretedFunction("indirCall", traceVar :: m.target :: Nil, traceType))))
    case m: DirectCall => ChangeTo(directCallFunc(m))
    case _ => SkipChildren()

  }
}

object PCMan {

  val allocatedPCS = mutable.Map[String, IntLiteral]()
  var pcCounter = 0
  def PCSym(s: String) = {
    allocatedPCS.getOrElseUpdate(
      s, {
        pcCounter += 1
        IntLiteral(pcCounter)
      }
    )
  }

  def setPCLabel(label: String) = {
    val pcVar = transitionSystemPCVar
    LocalAssign(pcVar, PCSym(label), Some(label))
  }

  def pcGuard(label: String) = {
    val pcVar = transitionSystemPCVar
    Assume(BinaryExpr(EQ, pcVar, PCSym(label)), Some(s"PC = $label"))
  }
}

import PCMan.*

def procToTransition(p: Procedure, loops: List[Loop], cutJoins: Boolean = false) = {

  val pcVar = transitionSystemPCVar
  var cutPoints = Map[String, Block]()

  val synthEntryJump = GoTo(Seq())
  val synthEntry = Block(s"${p.name}_SYNTH_ENTRY", None, Seq(), synthEntryJump)
  val synthExit = Block(s"${p.name}_SYNTH_EXIT")
  val synthAbort = Block(s"${p.name}_SYNTH_ABORT", None, Seq(setPCLabel("ABORT")), Return())

  p.addBlocks(Seq(synthEntry, synthExit, synthAbort))

  p.entryBlock.foreach(e => {
    e.statements.prepend(pcGuard("ENTRY"))
    synthEntryJump.addTarget(e)
    cutPoints = cutPoints.updated("ENTRY", e)
  })

  p.returnBlock.foreach(e => {
    e.jump match {
      case r: Return => {
        val outAssigns = r.outParams.map((formal, actual) => {
          val l = LocalAssign(formal, actual)
          l.comment = Some("synth return param")
          l
        })
        r.parent.statements.appendAll(outAssigns)
        r.parent.statements.append(setPCLabel("RETURN"))
        r.parent.replaceJump(GoTo(synthExit))
      }
      case _ => ???
    }

    cutPoints = cutPoints.updated("RETURN", e)
  })

  p.entryBlock = synthEntry
  p.returnBlock = synthExit

  var loopCount = 0

  for (l <- loops.filter(l => p.blocks.contains(l.header))) {
    var loopBECount = 0
    loopCount += 1
    // synthEntryJump.addTarget(l.header)
    // val loopEntry = Block(s"${p.name}LoopEntry$loopCount", None, Seq(pcGuard(s"Loop${loopCount}")), GoTo(l.header))
    // p.addBlock(loopEntry)
    // synthEntryJump.addTarget(loopEntry)

    val backedges = l.backEdges.toList.sortBy(e => s"${e.from.label}_${e.to.label}")
    val label = s"Loop${loopCount}"
    synthEntryJump.addTarget(l.header)

    val nb = synthEntry.createBlockBetween(l.header, "cut_join_to_" + label)
    nb.statements.prepend(pcGuard(label))

    cutPoints = cutPoints.updated(label, l.header)
    for (backedge <- backedges) {
      assert(l.header == backedge.to)
      backedge.from.statements.append(LocalAssign(pcVar, PCSym(label), Some(label)))
      backedge.from.replaceJump(GoTo(synthExit))
    }
  }

  var joinCount = 0
  if (cutJoins) {

    val cuts = p.blocks
      .filter(c =>
        c.prevBlocks.size > 1
          && c.prevBlocks.flatMap(_.nextBlocks).forall(_ == c)
          && !p.returnBlock.contains(c)
          && !p.entryBlock.contains(c)
      )
      .toList
      .sortBy(_.label)

    for (c <- cuts) {
      var incCount = 1
      joinCount = joinCount + 1
      val label = s"Join${joinCount}"
      cutPoints = cutPoints.updated(label, c)

      for (incoming <- c.prevBlocks) {
        incoming.statements.append(LocalAssign(pcVar, PCSym(label), Some(label)))
        incoming.replaceJump(GoTo(synthExit))
      }

      synthEntryJump.addTarget(c)
      val nb = synthEntry.createBlockBetween(c, "cut_join_to_" + label)
      nb.statements.prepend(pcGuard(label))

    }
  }

  var once = true
  for (s <- p) {
    s match {
      // case r: Return => {
      //  assert(once, "expect proc in single return form")
      // }
      case u: Unreachable => u.parent.replaceJump(GoTo(synthAbort))
      case g: GoTo if g.targets.isEmpty => g.parent.replaceJump(GoTo(synthAbort))
      // case a: Assert => {
      //  val start = a.parent
      //  val stmts = a.parent.statements.splitOn(a)
      //  val succ = a.parent.createBlockAfter("splitAssert")
      //  succ.statements.addAll(stmts)
      //  val trueBranch = start.createBlockBetween(succ)
      //  val falseBranch = start.createBlockBetween(succ)
      //  trueBranch.statements.prepend(Assume(a.body, Some(s"assert ${a.body}")))
      //  falseBranch.statements.prepend(Assume(UnaryExpr(BoolNOT,a.body), Some(s"assert failed ${a.body}" )))
      //  falseBranch.replaceJump(GoTo(synthAbort))
      //  start.statements.remove(a)
      // }
      case _ => ()
    }

  }
  synthExit.replaceJump(Return())
  synthAbort.replaceJump(Return())
  visit_proc(RewriteSideEffects(), p)
  cutPoints

}

/**
 * Converts each procedure to a transition system
 */
def toTransitionSystem(iprogram: Program) = {

  val program = IRToDSL.convertProgram(iprogram).resolve

  val loops = analysis.LoopDetector.identify_loops(program)
  val floops = loops.identifiedLoops.toList.sortBy(_.header.label)

  val cutPoints = program.procedures
    .map(p => {
      p -> procToTransition(p, floops)
    })
    .toMap

  (program, cutPoints)
}

/**
  * Clone p2 into p1, p2 
  *
  * Assumes block labels of p1 and p2 are disjoint
  */
def sequentialComposeTransitionSystems(p1prog: Program, p1: Procedure, p2: Procedure) = {

  val entry1 = p1.entryBlock.get
  val exit1 = p1.returnBlock.get

  // val entry2 = p2.entryBlock.get
  // val exit2 = p2.returnBlock.get

  val exitBB = IRToDSL.convertBlock(p2.returnBlock.get)
  val entryBB = IRToDSL.convertBlock(p2.entryBlock.get)
  val (exit2, rexit2) = exitBB.makeResolver
  val (entry2, rentry2) = entryBB.makeResolver

  val tempBlocks = (p2.blocks.toSet -- Seq(p2.entryBlock.get, p2.returnBlock.get)).map(IRToDSL.convertBlock)
  val temps = tempBlocks.map(v =>
    (
      v.makeResolver match {
        case (b, r) => b
      },
      v
    )
  )

  p1.addBlock(entry2)
  p1.addBlock(exit2)
  temps.foreach { case (b, r) =>
    p1.addBlock(b)
  }
  temps.foreach { case (b, r) =>
    r.cont(b)(p1prog, p1)
  }

  exitBB.cont(exit2)(p1prog, p1)
  entryBB.cont(entry2)(p1prog, p1)

  p1.returnBlock = exit2
  exit1.replaceJump(GoTo(entry2))
  exit2.replaceJump(Return())

  assert(invariant.cfgCorrect(p1prog))
  p1
}

enum Inv {
  case CutPoint(cutPointPCGuard: String, pred: Expr, comment: Option[String] = None)
  case Global(pred: Expr, comment: Option[String] = None)
}

class TranslationValidator {
  import Inv.*

  var initProg: Option[Program] = None

  var beforeProg: Option[Program] = None
  var liveBefore = Map[String, (Map[Block, Set[Variable]], Map[Block, Set[Variable]])]()
  // proc -> block -> absdom

  var afterProg: Option[Program] = None
  var liveAfter = Map[String, (Map[Block, Set[Variable]], Map[Block, Set[Variable]])]()

  var beforeCuts: Map[Procedure, Map[String, Block]] = Map()
  var afterCuts: Map[Procedure, Map[String, Block]] = Map()

  // proc -> List (pred, comment)

  extension (i: Inv) {
    def toAssume = i match {
      case CutPoint(label, pred, c) => {
        val guarded =
          BinaryExpr(BoolIMPLIES, exprInSource(BinaryExpr(EQ, transitionSystemPCVar, PCSym(label))), pred)
        Assume(guarded, c)
      }
      case Global(pred, c) => {
        Assume(pred, c)
      }
    }

    def toAssert = i.toAssume match {
      case Assume(b, c, _, _) => Assert(b, c)
    }
  }

  val invariants = mutable.Map[String, List[Inv]]()

  val beforeRenamer = NamespaceState("target")
  val afterRenamer = NamespaceState("source")

  def exprInSource(v: Expr) = visit_expr(afterRenamer, v)
  def exprInTarget(v: Expr) = visit_expr(beforeRenamer, v)
  def varInSource(v: Variable) = visit_rvar(afterRenamer, v)
  def varInTarget(v: Variable) = visit_rvar(beforeRenamer, v)

  def blockDone(b: Block) = {
    LocalVar(b.label + "_done", BoolType)
  }

  def extractProg(begin: Block): Iterable[Expr] = {

    var firstAssert = true
    var gotAssert = false
    var assumes = List[Expr]()
    var asserts = List[Expr]()
    var seen = Set[Block]()

    for (nb <- begin.forwardIteratorFrom) {
      nb match {
        case s: Assume => {
          assumes = s.body :: assumes
        }
        case s: Assert => {
          asserts = s.body :: asserts
        }
        case o: Jump => ()
        case o: Block => ()
        case o => {
          throw Exception(s"Program has other statements : $o")
        }
      }
      // assuming no cycles in graph
    }

    assumes ++ asserts
  }

  def ssaDAG(p: Procedure): ((Block, Command) => Command) = {

    val finalBlock = Block(p.name)

    var renameCount = 0
    var stRename = Map[Block, Map[Variable, Variable]]()
    // use for blocks

    var count = Map[String, Int]()

    def freshName(v: Variable) =
      renameCount = count.get(v.name).getOrElse(0) + 1
      count = count + (v.name -> renameCount)
      v match {
        case l: LocalVar => l.copy(varName = l.name + "_AT" + renameCount, index = 0)
        case l: GlobalVar => l.copy(name = l.name + "_AT" + renameCount)
      }

    class Subst(rn: Variable => Option[Variable]) extends CILVisitor {
      override def vexpr(e: Expr) = {
        ChangeTo(Substitute(rn, false)(e).getOrElse(e))
      }
    }

    def renameRHS(rename: Map[Variable, Variable])(c: Command): Command = c match {
      // rename all rvars
      case s: Statement => visit_stmt(Subst(rename.get), s).head
      case s: Jump => visit_jump(Subst(rename.get), s)
    }

    ir.transforms.reversePostOrder(p)
    val worklist = mutable.PriorityQueue[Block]()(Ordering.by(_.rpoOrder))
    worklist.addAll(p.blocks)

    class RenameLHS(subst: Variable => Option[Variable]) extends CILVisitor {
      override def vlvar(v: Variable) = subst(v) match {
        case Some(vn) => ChangeTo(vn)
        case none => SkipChildren()
      }
    }

    def renameLHS(substs: Map[Variable, Variable], s: Statement) = {
      visit_stmt(RenameLHS(substs.get), s)
    }

    while (worklist.nonEmpty) {
      val b = worklist.dequeue()
      var blockDoneCond = List[Expr](boolOr(b.prevBlocks.map(blockDone).toList))
      //val onlyOne = boolAnd(for {
      //  l <- b.prevBlocks
      //  r <- b.prevBlocks.filterNot(_ == l)
      //  neq = UnaryExpr(BoolNOT, BinaryExpr(BoolAND, blockDone(l), blockDone(r)))
      //} yield neq)

      var phis = Vector[Statement]()

      var renaming = if (b.prevBlocks.nonEmpty) then {
        var joinedRenames = Map[Variable, Variable]()
        val defines = b.prevBlocks.flatMap(b => stRename.get(b).map(b -> _).toSeq)
        var varToRenamings: Map[Variable, Iterable[(Block, Variable, Variable)]] =
          defines
            .flatMap { case (b, rns) =>
              rns.map { case (v, rn) =>
                (b, v, rn)
              }
            }
            .groupBy(_._2)
        var inter: Set[Variable] = varToRenamings.collect {
          case (v, defset) if defset.map(_._3).toSet.size > 1 => v
        }.toSet
        var disjoint: Map[Variable, Variable] = varToRenamings.collect {
          case (v, defset) if !inter.contains(v) => {
            assert(defset.tail.forall(_._3 == defset.head._3))
            (defset.head._2, defset.head._3)
          }
          // case (v, defset) if defset.size == 1 => (defset.head._2, defset.head._3)
        }.toMap
        var nrenaming: Map[Variable, Variable] = disjoint
        inter.foreach(v => {

          val defsToJoin =
            b.prevBlocks.filter(b => stRename.get(b).exists(_.contains(v)))
            // .flatMap(b => {
            //   stRename.get(b).flatMap(_.get(v)).map(v => b -> v).toSeq
            // })
            // .groupBy(_._2)
            // .map { case (v, blocks) =>
            //   v -> blocks.map(_._1).toSet
            // }

          val fresh = freshName(v)

          val oneOf = boolOr(defsToJoin.map(blockDone))
          val phicond = defsToJoin.map(b => {
            // val others = UnaryExpr(BoolNOT, boolOr(defsToJoin.filterNot(_ == b).map(blockDone)))
            BinaryExpr(BoolIMPLIES, blockDone(b), polyEqual(stRename(b)(v), fresh))
          })

          val phiscond = if (phicond.toList.length > 5) then {
            phicond.map(b => Assume(b))
          } else Seq(Assume(boolAnd(phicond)))
          phis = phis ++ phiscond
          // Assume(
          //  boolAnd(phicond),
          //  Some(s"${fresh.name} = phi(${defsToJoin.map(stRename(_)(v).name).mkString(",")})")
          // )

          joinedRenames = joinedRenames + (v -> fresh)
        })

        nrenaming = nrenaming ++ joinedRenames
        // val disj = (rhs.map(_._1).toSet ++ lhs.map(_._1).toSet) -- inter
        // val orenames = disj.map(v => v -> lhs.get(v).getOrElse(rhs(v))) ++ joinedRenames
        // stRename = stRename + (b -> orenames.toMap)

        stRename = stRename + (b -> nrenaming)
        b.statements.prependAll(phis)
        stRename(b)
      } else {
        stRename.get(b).getOrElse(Map())
      }

      for (s <- b.statements.toList) {
        val c = renameRHS(renaming)(s) // also modifies in-place
        c match {
          case a @ Assume(cond, _, _, _) if !phis.contains(a) =>
            blockDoneCond = cond :: blockDoneCond
            b.statements.remove(a)
          case a: Assign => {
            a.assignees.foreach(v => {
              val freshDef = freshName(v)
              renameLHS(Map(v -> freshDef), a)
              renaming = renaming + (v -> freshDef)
            })
          }
          case _ => ()
        }
      }

      renameRHS(renaming)(b.jump)
      stRename = stRename + (b -> renaming)

      val c =
        if (b.parent.entryBlock.contains(b) || b.label.endsWith("SYNTH_ENTRY")) then TrueLiteral
        else boolAnd(blockDoneCond)

      b.statements.append(LocalAssign(blockDone(b), c))
      if (b.label.contains("SYNTH_EXIT")) {
        b.statements.append(Assert(blockDone(b), Some("blockdone")))
      }
    }

    (b, c) => renameRHS(stRename(b))(c)
  }

  def setDSAInvariant = {

    val procs = initProg.get.procedures.view.map(p => p.name -> p).toMap

    for (p <- afterProg.get.procedures) {
      val (liveVarsTarget, _) = liveBefore(p.name)
      val (liveVarsSource, _) = liveAfter(p.name)

      /** has index **/

      def removeIndex(v: Variable) = v match {
        case l: LocalVar => l.copy(index = 0)
        case g => g
      }

      val returnInv = procs(p.name).returnBlock
        .map(_.jump match {
          case r: Return => procs(p.name).returnBlock.get.label -> r.outParams.map((formal, actual) => formal).toSet
          case _ => ???
        })
        .toSeq

      val lives = liveVarsSource.collect {
        case (block, v) if afterCuts(p).exists((_, b) => block.label == b.label) =>
          block.label -> v.filter(v => liveVarsTarget(block).contains(removeIndex(v)))
      }.toMap ++ returnInv

      val inv = afterCuts(p).map {
        case (label, cutPoint) => {
          val vars = lives.get(cutPoint.label).toSet.flatten -- Seq(transitionSystemPCVar)

          val assertion = boolAnd(vars.map(v => polyEqual(varInSource(v), varInTarget(removeIndex(v)))).toList)

          Inv.CutPoint(label, assertion, Some(s"INVARIANT at $label"))
        }
      }

      setInvariant(p.name, inv.toList)
    }

  }

  def setEqualVarsInvariant = {

    // call this after running transform so initProg corresponds to the source / after program.

    val procs = initProg.get.procedures.view.map(p => p.name -> p).toMap

    for (p <- afterProg.get.procedures) {
      val (liveVarsTarget, _) = liveAfter(p.name)
      val (liveVarsSource, _) = liveBefore(p.name)

      // intersect of live of source and target

      val returnInv = procs(p.name).returnBlock
        .map(_.jump match {
          case r: Return =>
            procs(p.name).returnBlock.get.label -> r.outParams.map((formal: Variable, actual) => formal -> formal).toMap
          case _ => ???
        })
        .toSeq

      // block -> sourcevar -> targetvar
      val lives: Map[String, Map[Variable, Variable]] = liveVarsSource.collect {
        case (block, v) if v != transitionSystemPCVar && afterCuts(p).exists((_, b) => block.label == b.label) =>
          block.label -> v.collect {
            case sv if liveVarsTarget(block).exists(_.name == sv.name) =>
              sv -> liveVarsTarget(block).find(_.name == sv.name).get
          }.toMap
      }.toMap ++ returnInv

      val inv = afterCuts(p).map {
        case (label, cutPoint) => {
          val vars = lives.get(cutPoint.label).getOrElse(Map())

          val assertion = boolAnd(
            vars
              .map { case (src, tgt) =>
                polyEqual(varInSource(src), varInTarget(tgt))
              }
          )

          // val guard = BinaryExpr(EQ, visit_rvar(afterRenamer, transitionSystemPCVar), PCMan.PCSym(label))

          Inv.CutPoint(label, assertion, Some(s"INVARIANT at $label"))
        }
      }

      setInvariant(p.name, inv.toList)
    }
  }

  val pcInv =
    BinaryExpr(EQ, visit_rvar(beforeRenamer, transitionSystemPCVar), visit_rvar(afterRenamer, transitionSystemPCVar))

  val traceInv = BinaryExpr(EQ, visit_rvar(beforeRenamer, traceVar), visit_rvar(afterRenamer, traceVar))

  def reachingDefs(p: Procedure) = {
    transforms.reversePostOrder(p)
    val (beforeLive, afterLive) = transforms.getLiveVars(p)
    val dom = transforms.DefUseDomain(beforeLive)
    val solver = transforms.worklistSolver(dom)
    val (beforeRes, afterRes) = solver.solveProc(p)
    beforeRes.map((k, v) => k.label -> v)
  }

  def setTargetProg(p: Program) = {
    initProg = Some(p)
    val (prog, cuts) = toTransitionSystem(p)
    liveBefore = p.procedures.map(p => p.name -> transforms.getLiveVars(p)).toMap
    beforeProg = Some(prog)
    beforeCuts = cuts
  }

  def setSourceProg(p: Program) = {
    val (prog, cuts) = toTransitionSystem(p)
    liveAfter = p.procedures.map(p => p.name -> transforms.getLiveVars(p)).toMap
    afterProg = Some(prog)
    afterCuts = cuts
  }

  def addInvariant(p: String, l: List[Inv]) = {
    invariants(p) = invariants(p) ++ l
  }

  def setInvariant(p: String, l: List[Inv]) = {
    invariants(p) = l
  }

  def addRDInvariant() = {
    val defsSource = initProg.get.procedures.map(p => p.name -> reachingDefs(p)).toMap
    beforeCuts
      .filter((proc, _) => defsSource.contains(proc.name))
      .foreach((proc, cuts) =>
        cuts.foreach {
          case (label, block) => {
            val rd = defsSource(proc.name)(block.label)
            val inv = rd.toSeq.flatMap { case (v, assigns) =>
              assigns.collect {
                case LocalAssign(lhs, rhs, _) => v -> rhs
                case s: SimulAssign =>
                  s.assignments.collect {
                    case (l, r) if l == v => l -> r
                  }.head
              }
            }
            val preds = inv.map { case (v, defin) =>
              polyEqual(v, defin)
            }
            if (preds.nonEmpty) {
              val pred = preds.reduce((l, r) => BinaryExpr(BoolAND, l, r))
              // val guarded =
              //  exprInSource(BinaryExpr(BoolIMPLIES, BinaryExpr(EQ, transitionSystemPCVar, PCSym(label)), pred))
              addInvariant(proc.name, List(Inv.CutPoint(label, pred, Some(s"Reaching Defs $label"))))
            }
          }
        }
      )
  }

  private class CollectUninterps extends CILVisitor {

    var funcs = List[UninterpretedFunction]()

    override def vexpr(e: Expr) =
      e match {
        case u: UninterpretedFunction =>
          funcs = u :: funcs
          DoChildren()
        case _ => DoChildren()
      }
  }

  private def getUninterps(p: Procedure) = {
    val v = CollectUninterps()
    visit_proc(v, p)
    v.funcs.map(u => u.name -> u).toMap
  }

  def getValidationProgWPConj = {

    val trueFun = FunctionDecl("TRUE", List(), BoolType, None, List("builtin" -> Some("\"true\"")))
    val falseFun = FunctionDecl("FALSE", List(), BoolType, None, List("builtin" -> Some("\"false\"")))

    var splitCandidates = Map[Procedure, ArrayBuffer[GoTo]]()

    var validationProcs = Map[String, Procedure]()
    var progs = List[Program]()

    val interesting = initProg.get.procedures
      .filterNot(_.isExternal.contains(true))
      .filterNot(_.procName.startsWith("indirect_call_launchpad"))

    for (proc <- interesting) {
      val after = afterProg.get.procedures.find(_.name == proc.name).get
      val before = beforeProg.get.procedures.find(_.name == proc.name).get
      val source = after
      val target = before

      visit_proc(AssertsToAssumes, source)

      val ufs = getUninterps(after)
      val uninterpfuncs = getUninterps(before).keys.toSet.intersect(ufs.keys.toSet).map(ufs(_))

      val uninterpAxioms =
        if (!linkUninterpByAxioms) then Set()
        else
          uninterpfuncs.map {
            case UninterpretedFunction(n, p, rt, _) => {

              val params = p.toList.zipWithIndex.map { case (p, i) =>
                LocalVar(s"arg$i", p.getType)
              }
              val srcParams = params.map(varInSource).map {
                case l: LocalVar => l
                case g: GlobalVar => throw Exception("should be fresh var")
              }
              val tgtParams = params.map(varInTarget).map {
                case l: LocalVar => l
                case g: GlobalVar => throw Exception("should be fresh var")
              }

              val lhs = boolAnd(params.map(r => polyEqual(exprInSource(r), exprInTarget(r))))
              val srccall = UninterpretedFunction("source__" + n, srcParams, rt, true)
              val tgtcall = UninterpretedFunction("target__" + n, tgtParams, rt, true)
              val rhs = polyEqual(srccall, tgtcall)
              val q = QuantifierExpr(
                QuantifierSort.forall,
                LambdaExpr(
                  srcParams.zip(tgtParams).flatMap((a, b) => List[LocalVar](a, b)),
                  BinaryExpr(BoolIMPLIES, lhs, rhs)
                ),
                List(srccall, tgtcall)
              )
              AxiomDecl(q)
            }
          }

      visit_proc(beforeRenamer, before)
      visit_proc(afterRenamer, after)

      // matches the smtlib let def emitted by boogie for the wp at the block
      val wpconjSourceFun =
        FunctionDecl(
          proc.name + "_source",
          List(),
          BoolType,
          None,
          List("builtin" -> Some("\"" + source.entryBlock.get.label + "_correct\""))
        )
      val wpconjTargetFun =
        FunctionDecl(
          proc.name + "_target",
          List(),
          BoolType,
          None,
          List("builtin" -> Some("\"" + target.entryBlock.get.label + "_correct\""))
        )

      val combined = IRToDSL.parTransitionSystems(afterProg.get, after, before)
      ir.transforms.reversePostOrder(combined)

      val ackermannTransforms = Ackermann.doTransform(combined, afterRenamer, beforeRenamer)
      val srce = combined.blocks.find(_.label == after.entryBlock.get.label).get
      val tgte = combined.blocks.find(_.label == before.entryBlock.get.label).get

      val ackInv = Ackermann.instantiateAxioms(srce, tgte, ackermannTransforms, false)
      // val ackInv = Ackermann.naiveInvariant(ackermannTransforms)
      visit_proc(Ackermann.ToAssume(ackermannTransforms), combined)

      // addInvariant(proc.name, ackInv.map(v => (v, Some("ackermannisation"))))

      val prime = NamespaceState("P")

      // add inv on transition system
      combined.blocks.find(_.label == after.entryBlock.get.label).get.statements.append(Assume(trueFun.makeCall()))
      combined.blocks.find(_.label == before.entryBlock.get.label).get.statements.append(Assume(trueFun.makeCall()))
      val invVariables = invariants(proc.name).flatMap(_.toAssume.body.variables).toSet
      val QSource =
        val sourceInvVariables =
          invVariables.filter(_.name.startsWith(afterRenamer.namespace)) ++ Seq(transitionSystemPCVar, traceVar).map(
            visit_rvar(afterRenamer, _)
          )
        val vars = sourceInvVariables.map(v => polyEqual(visit_expr(prime, v), v))
        boolAnd(vars)

      val QTarget =
        val targetInvVariables =
          invVariables.filter(_.name.startsWith(beforeRenamer.namespace)) ++ Seq(transitionSystemPCVar, traceVar).map(
            visit_rvar(beforeRenamer, _)
          )
        val vars = targetInvVariables.map(v => polyEqual(visit_expr(prime, v), v))
        boolAnd(vars)

      combined.blocks
        .find(_.label == after.returnBlock.get.label)
        .get
        .statements
        .append(Assert(UnaryExpr(BoolNOT, QSource)))
      combined.blocks
        .find(_.label == before.returnBlock.get.label)
        .get
        .statements
        .append(Assert(UnaryExpr(BoolNOT, QTarget)))

      // add invariant to combined
      val initInv = Seq(Inv.Global(pcInv, Some("PC INVARIANT")), Inv.Global(traceInv, Some("Trace INVARIANT")))
      val invariant = initInv ++ invariants(proc.name)
      val primedInv = (initInv ++ invariants(proc.name)).map(_.toAssume).map { case Assume(b, c, _, _) =>
        Assert(visit_expr(prime, b), c)
      }
      val proof =
        invariant.map(i => i.toAssume)
          ++ List(
            Assume(UnaryExpr(BoolNOT, wpconjSourceFun.makeCall()), Some(" wp conjugate of source program")),
            Assume(UnaryExpr(BoolNOT, wpconjTargetFun.makeCall()), Some(" wp conjugate of target program"))
          )
          ++ primedInv
          ++ List(Assume(falseFun.makeCall()))

      combined.entryBlock.get.statements.prependAll(proof)
      combined.entryBlock.get.statements.prependAll(ackInv.map(a => Assume(a, Some("ackermann"))))
      validationProcs = validationProcs.updated(proc.name, combined)

      val internalLabels = before.blocks.map(_.label).toSet ++ after.blocks.map(_.label)

      val splitJumps = combined.blocks.map(_.jump).collect {
        case g: GoTo if g.targets.size > 1 && internalLabels.contains(g.parent.label) => g
      }
      splitCandidates = splitCandidates.updated(combined, ArrayBuffer.from(splitJumps))

      val decls = (List(trueFun, falseFun, wpconjTargetFun, wpconjSourceFun))
      val bidx = afterProg.get.procedures.indexOf(before)

      progs = Program(ArrayBuffer(combined), combined, afterProg.get.initialMemory, ArrayBuffer.from(decls)) :: progs
      // beforeProg.get.procedures.remove(bidx)
    }

    (progs, splitCandidates)
  }

  /**
   * Generate an SMT query for the product program, 
   * !! Assume source and taregt in class scope are already renamed
   *
   * Returns a map from proceudre -> smt query
   */
  def getValidationSMT: Map[String, String] = {
    // assume already renamed

    var smtQueries = Map[String, String]()

    var splitCandidates = Map[Procedure, ArrayBuffer[GoTo]]()

    var progs = List[Program]()

    val interesting = initProg.get.procedures
      .filterNot(_.isExternal.contains(true))
      .filterNot(_.procName.startsWith("indirect_call_launchpad"))

    for (proc <- interesting) {
      val source = afterProg.get.procedures.find(_.name == proc.name).get
      val target = beforeProg.get.procedures.find(_.name == proc.name).get

      val srce = source.entryBlock.get
      val tgte = target.entryBlock.get
      val srcExit = source.returnBlock.get
      val tgtExit = target.returnBlock.get

      val prime = NamespaceState("P")

      val invVariables = invariants(proc.name).flatMap(_.toAssume.body.variables).toSet

      val QSource =
        val sourceInvVariables =
          invVariables.filter(_.name.startsWith(afterRenamer.namespace)) ++ Seq(transitionSystemPCVar, traceVar).map(
            visit_rvar(afterRenamer, _)
          )
        val vars = sourceInvVariables.map(v => polyEqual(visit_expr(prime, v), v))
        boolAnd(vars)

      val QTarget =
        val targetInvVariables =
          invVariables.filter(_.name.startsWith(beforeRenamer.namespace)) ++ Seq(transitionSystemPCVar, traceVar).map(
            visit_rvar(beforeRenamer, _)
          )
        val vars = targetInvVariables.map(v => polyEqual(visit_expr(prime, v), v))
        boolAnd(vars)

      srce.statements.prepend(LocalAssign(varInSource(transitionSystemPCVar), varInSource(transitionSystemPCVar)))
      srce.statements.prepend(LocalAssign(varInSource(traceVar), varInSource(traceVar)))
      tgte.statements.prepend(LocalAssign(varInTarget(transitionSystemPCVar), varInTarget(transitionSystemPCVar)))
      tgte.statements.prepend(LocalAssign(varInTarget(traceVar), varInTarget(traceVar)))

      val srcRename = ssaDAG(source)
      val tgtRename = ssaDAG(target)

      // add invariant to combined
      val initInv = Seq(Inv.Global(pcInv, Some("PC INVARIANT")), Inv.Global(traceInv, Some("Trace INVARIANT")))
      val invariant = initInv ++ invariants(proc.name).map {
        case g: Inv.Global => g
        case i @ CutPoint(l, b, comment) => {
          val targetBl = beforeCuts(target)(l)
          val sourceBl = afterCuts(source)(l)
          val inv1 = tgtRename(targetBl, Assume(b))
          val inv2 = srcRename(sourceBl, inv1) match {
            case a: Assume => a.body
            case _ => ???
          }
          CutPoint(l, inv2, comment)
        }
      }
      val primedInv = (initInv ++ invariants(proc.name)).map(_.toAssume).map { case Assume(b, c, _, _) =>
        Assert(visit_expr(prime, b), c)
      }

      val ack = Ackermann.getVisitor(afterRenamer, beforeRenamer)
      visit_proc(ack, source)
      visit_proc(ack, target)
      val ackInv = Ackermann.instantiateAxioms(source.entryBlock.get, target.entryBlock.get, ack.axioms, true).toSet
      visit_proc(Ackermann.ToAssume(ack.axioms), source)
      visit_proc(Ackermann.ToAssume(ack.axioms), target)

      Ackermann.passify(source)
      Ackermann.passify(target)

      val qsrc = srcRename(srcExit, Assume(QSource)).asInstanceOf[Assume].body
      val qtrgt = tgtRename(tgtExit, Assume(QTarget)).asInstanceOf[Assume].body
      val otargets = srce.jump.asInstanceOf[GoTo].targets.toList

      // case split proof

      val splits = srce.forwardIteratorFrom.collect {
        case g: GoTo if (!g.targets.exists((b: Block) => b.label.contains("SYNTH_EXIT"))) => g -> g.targets.toList
      }.toList

      val splitMap = splits.toMap

      val splitsToApply = ir.transforms.chooseSplits(splits, 4).zipWithIndex

      for ((split, splitNo) <- splitsToApply) {
        val splitName = proc.name + "_split_" + splitNo

        // Logger.writeToFile(File(s"graphs/splittv-${splitName}.dot"), dotBlockGraph(combined.blocks.toList, Set()))

        // build smt query
        val b = translating.BasilIRToSMT2.Builder()

        var count = 0
        for ((gt, tgts) <- split) {
          if (tgts.size < splitMap(gt).size) {
            count += 1
            b.addAssert(boolAnd(tgts.map(blockDone(_))), Some(s"forceSplit${splitNo}Jump${count}"))
          }
        }

        count = 0
        for (i <- invariant) {
          count += 1
          b.addAssert(i.toAssume.body, Some(s"inv$count"))
        }
        count = 0
        for (i <- ackInv) {
          count += 1
          b.addAssert(i, Some(s"ack$count"))

        }
        count = 0
        for (i <- extractProg(srce)) {
          count += 1
          b.addAssert(i, Some(s"source$count"))
        }
        count = 0
        b.addAssert(qsrc, Some("Qsrc"))
        for (i <- extractProg(tgte)) {
          count += 1
          b.addAssert(i, Some(s"tgt$count"))
        }
        b.addAssert(qtrgt, Some("Qtgt"))
        b.addAssert(UnaryExpr(BoolNOT, BoolExp(BoolAND, primedInv.map(_.body).toList)), Some("InvPrimed"))

        smtQueries = smtQueries + (splitName -> b.getCheckSat())

      }

    }

    smtQueries
  }

  def getValidationProg = {
    var validationProcs = Map[String, Procedure]()

    for (proc <- initProg.get.procedures) {
      val after = afterProg.get.procedures.find(_.name == proc.name).get
      val before = beforeProg.get.procedures.find(_.name == proc.name).get

      visit_proc(beforeRenamer, before)
      visit_proc(afterRenamer, after)

      val combined = sequenceTransitionSystems(afterProg.get, after, before)
      val invariant =
        Seq(Inv.Global(pcInv, Some("PC INVARIANT")), Inv.Global(traceInv, Some("Trace INVARIANT"))) ++ invariants(
          proc.name
        )

      combined.entryBlock.get.statements.prependAll(invariant.map(_.toAssume))
      combined.returnBlock.get.statements.appendAll(invariant.map(_.toAssert))

      validationProcs = validationProcs.updated(proc.name, combined)

      val bidx = afterProg.get.procedures.indexOf(before)
      // beforeProg.get.procedures.remove(bidx)
    }

    // TODO: fix when implementing calls
    val interesting = validationProcs.map(_._2)
    afterProg.get.procedures = ArrayBuffer.from(interesting)

    afterProg.get
  }

}
