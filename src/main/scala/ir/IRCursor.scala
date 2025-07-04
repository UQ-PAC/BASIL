package ir
import cfg_visualiser.{DotArrow, DotGraph, DotInterArrow, DotNode, DotRegularArrow}
import translating.BasilIRPrettyPrinter
import translating.PrettyPrinter.*
import util.assertion.*

import scala.annotation.tailrec

import collection.mutable

/** 
 *  This file defines functions to get the successor and predecessor of a IR node for control flow.
 */

/*
 * Defines a position in the IL / CFG; this becomes the lhs of the state map lattice in a static analysis.
 */
type CFGPosition = Procedure | Block | Command

def isAfterCall(c: Command) = {
  (IRWalk.prevCommandInBlock(c)) match {
    case Some(c: Call) => true
    case _ => false
  }
}

extension (p: CFGPosition)
  def toShortString: String =
    p match
      case procedure: Procedure => procedure.toString
      case block: Block => s"Block ${block.label}"
      case command: Command => command.toString

trait Walk[IN, NT] {
  def succ(pos: IN): Set[NT]
  def pred(pos: IN): Set[NT]
}

trait SCCWalk[IN <: CFGPosition, NT <: CFGPosition & IN] extends Walk[Set[IN], Set[NT]]

// todo: we could just use the dependencies trait directly instead to avoid the instantiation issue
trait IRWalk[IN <: CFGPosition, NT <: CFGPosition & IN] extends Walk[IN, NT]

object IRWalk:

  def prevCommandInBlock(c: Command): Option[Command] = c match {
    case s: Statement => c.parent.statements.prevOption(s)
    case j: Jump => c.parent.statements.lastOption
  }

  def nextCommandInBlock(c: Command): Option[Command] = c match {
    case s: Statement => Some(s.successor)
    case j: Jump => None
  }

  def procedure(pos: CFGPosition): Procedure = {
    pos match {
      case p: Procedure => p
      case b: Block => b.parent
      case c: Command => c.parent.parent
    }
  }

  def blockBegin(pos: CFGPosition): Option[Block] = {
    pos match {
      case p: Procedure => p.entryBlock
      case b: Block => Some(b)
      case c: Command => Some(c.parent)
    }
  }

  def commandBegin(pos: CFGPosition): Option[Command] = {
    pos match {
      case p: Procedure => p.entryBlock.map(b => b.statements.headOption.getOrElse(b.jump))
      case b: Block => Some(b.statements.headOption.getOrElse(b.jump))
      case c: Command => Some(c)
    }
  }

  def lastInBlock(p: Block): Command = p.jump
  def firstInBlock(p: Block): Command = p.statements.headOption.getOrElse(p.jump)

  def firstInProc(p: Procedure): Option[Command] = p.entryBlock.map(firstInBlock)
  def lastInProc(p: Procedure): Option[Command] = p.returnBlock.map(lastInBlock)

// extension (p: Block)
//   def isProcEntry: Boolean = p.parent.entryBlock.contains(p)
//   def isProcReturn: Boolean = p.parent.returnBlock.contains(p)
//
//   def begin: CFGPosition = p
//   def end: CFGPosition = p.jump
//
// extension (p: Procedure)
//   def begin: CFGPosition = p
//   def end: CFGPosition = p.returnBlock.map(_.end).getOrElse(p)

/** Does not include edges between procedures.
  */
trait IntraProcIRCursor extends IRWalk[CFGPosition, CFGPosition] {

  def succ(pos: CFGPosition): Set[CFGPosition] = {
    pos match {
      case proc: Procedure => proc.entryBlock.toSet
      case b: Block => b.statements.headOption.orElse(Some(b.jump)).toSet
      case n: GoTo => n.targets.asInstanceOf[Set[CFGPosition]]
      case h: Unreachable => Set()
      case h: Return => Set()
      case c: Statement => IRWalk.nextCommandInBlock(c).toSet
    }
  }

  def pred(pos: CFGPosition): Set[CFGPosition] = {
    pos match {
      case c: Command => Set(IRWalk.prevCommandInBlock(c).getOrElse(c.parent))
      case b: Block if b.isEntry => Set(b.parent)
      case b: Block => b.incomingJumps.asInstanceOf[Set[CFGPosition]]
      case proc: Procedure => Set() // intraproc
    }
  }
}

object IntraProcIRCursor extends IntraProcIRCursor

trait IntraProcBlockIRCursor extends IRWalk[CFGPosition, Block] {

  @tailrec
  final def succ(pos: CFGPosition): Set[Block] = {
    pos match {
      case b: Block => b.nextBlocks.toSet
      case s: Command => succ(s.parent)
      case s: Procedure => s.entryBlock.toSet
    }
  }

  @tailrec
  final def pred(pos: CFGPosition): Set[Block] = {
    pos match {
      case b: Block if b.isEntry => Set.empty
      case b: Block => b.incomingJumps.map(_.parent).toSet
      case j: Command => pred(j.parent)
      case s: Procedure => Set.empty
    }
  }
}
object IntraProcBlockIRCursor extends IntraProcBlockIRCursor

/** Includes all intraproc edges as well as edges between procedures.
  *
  * forwards: Direct call -> target return indirect Call -> the procedure return block for all possible direct-call
  * sites
  *
  * backwards: Procedure -> all possible direct-call sites Call-return block -> return Call of the procedure called
  */
trait InterProcIRCursor extends IRWalk[CFGPosition, CFGPosition] {

  final def succ(pos: CFGPosition): Set[CFGPosition] = {
    pos match
      case c: DirectCall if c.target.blocks.nonEmpty => Set(c.target)
      case c: Return => c.parent.parent.incomingCalls().map(_.successor).toSet
      case _ => IntraProcIRCursor.succ(pos)
  }

  final def pred(pos: CFGPosition): Set[CFGPosition] = {
    pos match
      case c: Command =>
        IRWalk.prevCommandInBlock(c) match {
          case Some(d: DirectCall) if d.target.blocks.nonEmpty => d.target.returnBlock.toSet
          case o => o.toSet ++ IntraProcIRCursor.pred(pos)
        }
      case c: Procedure => c.incomingCalls().toSet.asInstanceOf[Set[CFGPosition]]
      case _ => IntraProcIRCursor.pred(pos)
  }
}

object InterProcIRCursor extends InterProcIRCursor

trait CallGraph extends IRWalk[Procedure, Procedure] {
  final def succ(b: Procedure): Set[Procedure] = b.calls

  final def pred(b: Procedure): Set[Procedure] = b.incomingCalls().map(_.parent.parent).toSet
}

object CallGraph extends CallGraph

/** Updates each procedure with its corresponding call graph scc
 *  if a procedure is the only member in the scc only
 *  update procedure if there is a self cycle in the procedure
 */
def updateWithCallSCC(program: Program): Unit = {
  val sccs = stronglyConnectedComponents(CallGraph, program.procedures)
  for (scc <- sccs) {
    if scc.size > 1 || scc.head.calls.contains(scc.head) then {
      scc.foreach(_.scc = Some(scc))
    }
  }
}

/** Walker over the Call graph SCCs
  * Ignores any edges from the SCC to itself
  * that is the scc will never be a pred or succ of itself
  */
object CallSCCWalker extends SCCWalk[Procedure, Procedure] {
  override def succ(scc: Set[Procedure]): Set[Set[Procedure]] = {
    // remove the scc corresponding to b from predecessor  list
    scc.flatMap(a => CallGraph.succ(a)).map(p => p.scc.getOrElse(Set(p))) - scc
  }
  override def pred(scc: Set[Procedure]): Set[Set[Procedure]] = {
    // remove the scc corresponding to b from predecessor  list
    scc.flatMap(a => CallGraph.pred(a)).map(p => p.scc.getOrElse(Set(p))) - scc
  }

  def succ(p: Procedure): Set[Set[Procedure]] = succ(p.scc.getOrElse(Set(p)))
  def pred(p: Procedure): Set[Set[Procedure]] = pred(p.scc.getOrElse(Set(p)))
}

// object InterProcBlockIRCursor extends InterProcBlockIRCursor

/** Computes the reachability transitive closure of the CFGPositions in initial under the successor relation defined by
  * walker.
  */
def computeDomain[T <: CFGPosition, O <: T](walker: IRWalk[T, O], initial: IterableOnce[O]): mutable.Set[O] = {
  val domain: mutable.Set[O] = mutable.Set.from(initial)
  val added: mutable.Set[O] = mutable.Set()

  var sizeBefore = 0
  var sizeAfter = domain.size
  while (sizeBefore != sizeAfter) {
    for (i <- domain) {
      added.addAll(walker.succ(i))
      added.addAll(walker.pred(i))
    }
    domain.addAll(added)
    added.clear()
    sizeBefore = sizeAfter
    sizeAfter = domain.size
  }
  domain
}

/** Compute the set of strongly connected subcomponents (flattened) in a topological sort order using
 *  Tarjan's strongly connected components algorithm
 */
def stronglyConnectedComponents[T <: CFGPosition, O <: T](
  walker: IRWalk[T, O],
  initial: IterableOnce[O]
): List[Set[O]] = {
  var index = 0;
  var stack = mutable.Stack[O]()
  var vIndex = mutable.Map[O, Int]()
  var vLowLink = mutable.Map[O, Int]()
  var vOnStack = mutable.Map[O, Boolean]()
  var out = mutable.ListBuffer[mutable.Set[O]]()

  for (proc <- computeDomain(walker, initial)) {
    if (!vIndex.contains(proc)) {
      strongconnect(proc)
    }
  }

  def strongconnect(cur: O): Unit = {
    vIndex(cur) = index
    vLowLink(cur) = index
    index += 1
    stack.push(cur)
    vOnStack(cur) = true

    for (next <- walker.succ(cur)) {
      if (!vIndex.contains(next)) {
        strongconnect(next)
        vLowLink(cur) = vLowLink(cur).min(vLowLink(next))
      } else if (vOnStack(next)) {
        vLowLink(cur) = vLowLink(cur).min(vIndex(next))
      }
    }

    if (vLowLink(cur) == vIndex(cur)) {
      var component = mutable.Set[O]()
      while { // do {
        val next = stack.pop()
        vOnStack(next) = false
        component += next
        next != cur // } while (next != cur)
      } do ()
      out += component
    }
  }
  debugAssert(stack.size == 0)

  out.map(_.toSet).toList
}

def toDot(program: Program, labels: Map[CFGPosition, String] = Map.empty, inter: Boolean = false): String = {
  if (inter) {
    val domain = computeDomain[CFGPosition, CFGPosition](InterProcIRCursor, program.procedures).toSet
    toDot[CFGPosition](domain, InterProcIRCursor, labels, Set())
  } else {
    val domain = computeDomain[CFGPosition, CFGPosition](IntraProcIRCursor, program.procedures).toSet
    toDot[CFGPosition](domain, IntraProcIRCursor, labels, Set())
  }
}

def dotCallGraph(program: Program, labels: Map[CFGPosition, String] = Map.empty): String = {
  val domain = computeDomain[Procedure, Procedure](CallGraph, program.procedures)
  toDot[Procedure](domain.toSet, CallGraph, labels, Set())
}

case class DetachedEntry(blocksEmptyPred: Set[Block], reachableFromBlockEmptyPred: Set[Block])

def getDetachedBlocks(p: Procedure) = {
  val b = p.blocks.filter(b => !p.entryBlock.contains(b) && b.prevBlocks.isEmpty)

  var oldReachable = Set[Block]()
  var reachable = b.toSet

  while (oldReachable != reachable) {
    oldReachable = reachable

    reachable = reachable.flatMap(b => Set(b) ++ b.nextBlocks)
  }

  DetachedEntry(b.toSet, reachable)
}

def dotBlockGraph(proc: Procedure): String = {
  val o = getDetachedBlocks(proc)
  dotBlockGraph(proc.blocks.toList, o.reachableFromBlockEmptyPred)
}

def dotBlockGraph(prog: Program): String = {
  val e = prog.procedures.toSet.flatMap(getDetachedBlocks(_).reachableFromBlockEmptyPred)

  dotBlockGraph(
    prog.collect { case b: Block =>
      b
    },
    e
  )
}

def dotFlowGraph(blocks: Iterable[Block], orphaned: Set[Block]): String = {
  val labels: Map[CFGPosition, String] = Map()
  toDot[Block](blocks.toSet, IntraProcBlockIRCursor, labels, orphaned)
}

def dotBlockGraph(blocks: Iterable[Block], orphaned: Set[Block]): String = {
  val printer = translating.BasilIRPrettyPrinter()
  val labels: Map[CFGPosition, String] = (blocks.collect { case b: Block =>
    b -> {
      (b.statements.toList.map(printer.apply(_) + ";") ++ {
        b.jump match {
          case g: GoTo => List()
          case o => List(printer(o) + ";")
        }
      }).map("  " + _).mkString("\n")
    }
  }).toMap

  toDot[Block](blocks.toSet, IntraProcBlockIRCursor, labels, orphaned)
}

def dotBlockGraph(program: Procedure, labels: Map[CFGPosition, String]): String = {
  val domain = computeDomain[CFGPosition, Block](IntraProcBlockIRCursor, program.blocks.toSet)
  val o = getDetachedBlocks(program).reachableFromBlockEmptyPred
  toDot[Block](domain.toSet, IntraProcBlockIRCursor, labels, o)
}

def dotBlockGraph(program: Program, labels: Map[CFGPosition, String] = Map.empty): String = {
  val domain = computeDomain[CFGPosition, Block](IntraProcBlockIRCursor, program.procedures.flatMap(_.blocks).toSet)
  val e = program.procedures.toSet.flatMap(getDetachedBlocks(_).reachableFromBlockEmptyPred)
  toDot[Block](domain.toSet, IntraProcBlockIRCursor, labels, e)
}

def toDot[T <: CFGPosition](
  domain: Set[T],
  iterator: IRWalk[? >: T, T],
  labels: Map[CFGPosition, String],
  filled: Set[T]
): String = {

  val visited: mutable.Set[CFGPosition] = mutable.Set.from(domain)
  var labelcounter = 0

  def label(l: Option[String]) = {
    l match {
      case Some(s) => s
      case None =>
        labelcounter += 1
        s"node$labelcounter"
    }
  }

  val dotNodes = mutable.Map[CFGPosition, DotNode]()
  val dotArrows = mutable.ListBuffer[DotArrow]()

  def nodeText(node: CFGPosition): String = {
    var text = node match {
      case s: Block => f"[Block] ${s.label} ${s.rpoOrder}"
      case s => s.toString
    }
    if (labels.contains(node)) {
      text = text + "\n\n" + labels(node)
    }
    text
  }

  for (node <- domain) {
    node match
      case s: Command => dotNodes.addOne(s -> DotNode(label(s.label), nodeText(s), filled.contains(node)))
      case s: Block => dotNodes.addOne(s -> DotNode(label(Some(s.label)), nodeText(s), filled.contains(node)))
      case s => dotNodes.addOne(s -> DotNode(label(Some(s.toString)), nodeText(s), filled.contains(node)))
  }

  def getArrow(s: CFGPosition, n: CFGPosition) = {

    if (!dotNodes.contains(n)) {
      val r = n match {
        case p: Block => (BasilIRPrettyPrinter()(p))
        case p: Statement => (BasilIRPrettyPrinter()(p))
        case _ => s"UNK: $n"
      }
      dotNodes(n) = DotNode(n.toString, r, true)

    }
    if (IRWalk.procedure(n) eq IRWalk.procedure(s)) {
      DotRegularArrow(dotNodes(s), dotNodes(n))
    } else {
      DotInterArrow(dotNodes(s), dotNodes(n))
    }
  }

  for (node <- domain) {
    node match {
      case s =>
        assert(dotNodes.contains(s))
        iterator.succ(s).foreach(n => dotArrows.addOne(getArrow(s, n)))
      //       iterator.pred(s).foreach(n => dotArrows.addOne(getArrow(s,n)))
    }
  }

  val allNodes = dotNodes.values.toList.sortBy(n => n.id)
  new DotGraph("CursorCFG", allNodes, dotArrows).toDotString
}

/**
 * This doesn't implement free vars for block or proc, it just returns the rvars.
 */
def freeVarsPos(s: CFGPosition): Set[Variable] = s match {
  case SimulAssign(assigns, _) => assigns.toSet.flatMap(_._2.variables)
  case a: MemoryAssign => a.rhs.variables
  case l: MemoryLoad => l.index.variables
  case a: MemoryStore => a.index.variables ++ a.value.variables
  case a: Assert => a.body.variables
  case a: Assume => a.body.variables
  case a: IndirectCall => a.target.variables
  case p: Procedure =>
    p.flatMap {
      case c: Command => freeVarsPos(c)
      case _ => Set()
    }.toSet
  case p: Block => p.statements.flatMap(freeVarsPos).toSet
  case p: DirectCall => p.actualParams.flatMap(_._2.variables).toSet
  case p: Return => p.outParams.flatMap(_._2.variables).toSet
  case _: Unreachable | _: GoTo | _: NOP => Set[Variable]()
}

def allVarsPos(s: CFGPosition): Set[Variable] = s match {
  case a: Assign => freeVarsPos(a) ++ a.assignees
  case p: Procedure =>
    p.flatMap {
      case c: Command => allVarsPos(c)
      case _ => Set()
    }.toSet
  case p: Block => p.statements.flatMap(allVarsPos).toSet
  case o => freeVarsPos(o)
}
