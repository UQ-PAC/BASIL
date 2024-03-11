package ir
import util.Logger
import cfg_visualiser.DotElement
import cfg_visualiser.{DotArrow, DotGraph, DotInlineArrow, DotInterArrow, DotIntraArrow, DotNode, DotRegularArrow}

import collection.mutable
import scala.annotation.tailrec

/** This file defines functions to get the successor and predecessor of a IR node for control flow.
  */

/*
 * Defines a position in the IL / CFG; this becomes the lhs of the state map lattice in a static analysis.
 */
type CFGPosition = Procedure | Block | Command

extension (p: CFGPosition)
  def toShortString: String =
    p match
      case procedure: Procedure => procedure.toString
      case block: Block => s"Block ${block.label}"
      case command: Command => command.toString

// todo: we could just use the dependencies trait directly instead to avoid the instantiation issue
trait IRWalk[IN <: CFGPosition, NT <: CFGPosition & IN] {
  def succ(pos: IN): Set[NT]
  def pred(pos: IN): Set[NT]
}

object IRWalk:
  def procedure(pos: CFGPosition): Procedure = {
    pos match {
      case p: Procedure => p
      case b: Block => b.parent
      case c: Command => c.parent.parent
    }
  }

  def blockBegin(pos: CFGPosition): Block = {
    pos match {
      case p: Procedure => p.entryBlock
      case b: Block => b
      case c: Command => c.parent
    }
  }

  def commandBegin(pos: CFGPosition): Option[Command] = {
    pos match {
      case p: Procedure => Some(p.entryBlock.statements.headOption.getOrElse(p.entryBlock.jump))
      case b: Block => Some(b.statements.headOption.getOrElse(b.jump))
      case c: Command => Some(c)
    }
  }

extension (p: Block) 
  def isProcEntry: Boolean =  p.parent.entryBlock eq p
  def isProcReturn: Boolean = p.parent.returnBlock eq p
  // TODO: this method doesn't require aftercall blocks only have 1 incoming jump 
  def isAfterCall: Boolean = p.incomingJumps.nonEmpty && p.incomingJumps.forall(_.isAfterCall)

  def begin: CFGPosition = p
  def end: CFGPosition = p.jump

extension (p: Command)
  def isAfterCall: Boolean = p match
    case g: GoTo => g.parent.fallthrough.contains(g)
    case _ => false

extension (p: Procedure)
  def begin: CFGPosition = p
  def end: CFGPosition = p.returnBlock.end

/**
 * Does not include edges between procedures.
 */
trait IntraProcIRCursor extends IRWalk[CFGPosition, CFGPosition] {

  def succ(pos: CFGPosition): Set[CFGPosition] = {
    pos match {
      case p: Procedure => Set(p.entryBlock)
      case b: Block     => Set(b.statements.headOption.getOrElse(b.jump))
      case s: Statement => Set(s.succ.getOrElse(s.parent.jump))
      case n: GoTo      => n.targets.asInstanceOf[Set[CFGPosition]]
      case c: Call      => c.parent.fallthrough.toSet
      case r: Return    => Set()
    }
  }

  def pred(pos: CFGPosition): Set[CFGPosition] = {
    pos match {
      case s: Statement => Set(s.pred.getOrElse(s.parent))
      case j: GoTo if j.isAfterCall => Set(j.parent.jump)
      case j: Jump => Set(j.parent.statements.lastOption.getOrElse(j.parent))
      case b: Block if b.isProcEntry => Set(b.parent)
      case b: Block => b.incomingJumps.asInstanceOf[Set[CFGPosition]]
      case p: Procedure => Set() // intraproc
    }
  }
}

object IntraProcIRCursor extends IntraProcIRCursor

trait IntraProcBlockIRCursor extends IRWalk[CFGPosition, Block] {

  @tailrec
  final def succ(pos: CFGPosition): Set[Block] = {
    pos match {
      case b: Block     => b.nextBlocks.toSet
      case s: Command   => succ(s.parent)
      case s: Procedure => Set(s.entryBlock)
    }
  }

  @tailrec
  final def pred(pos: CFGPosition): Set[Block] = {
    pos match {
      case b: Block if b.isProcEntry => Set.empty
      case b: Block => b.incomingJumps.map(_.parent)
      case j: Command => pred(j.parent)
      case s: Procedure => Set.empty
    }
  }
}

object IntraProcBlockIRCursor extends IntraProcBlockIRCursor

/**
 * Includes all intraproc edges as well as edges between procedures.
 *
 * forwards:
 * Direct call -> target
 * return indirect Call -> the procedure return block for all possible direct-call sites
 *
 * backwards:
 * Procedure -> all possible direct-call sites
 * Call-return block -> return Call of the procedure called
 *
 */
trait InterProcIRCursor extends IRWalk[CFGPosition, CFGPosition] {

  final def succ(pos: CFGPosition): Set[CFGPosition] = {
  IntraProcIRCursor.succ(pos) ++
    (pos match
      case c: DirectCall if c.target.blocks.nonEmpty  => Set(c.target)
      case c: IndirectCall if c.parent.isProcReturn => c.parent.parent.incomingCalls().flatMap(_.parent.fallthrough.toSet).toSet
      case _ =>  Set.empty)
  }

  final def pred(pos: CFGPosition): Set[CFGPosition] = {
    IntraProcIRCursor.pred(pos) ++
    (pos match
      case c: Procedure => c.incomingCalls().toSet.asInstanceOf[Set[CFGPosition]]
      case b: GoTo if b.isAfterCall => b.parent.jump match {
        case DirectCall(t, _, _) if t.blocks.nonEmpty => Set(t.returnBlock)
        case _ => Set(b)
      }
      case _ => Set.empty)
  }
}

trait InterProcBlockIRCursor extends IRWalk[CFGPosition, Block] {

  final def succ(pos: CFGPosition): Set[Block] = {
    IntraProcBlockIRCursor.succ(pos) ++
    (pos match {
      case s: DirectCall if s.target.blocks.nonEmpty => Set(s.target.entryBlock)
      case b: Block if b.isProcReturn => b.parent.incomingCalls().map(_.parent).toSet
      case _ => Set.empty
    })
  }

  final def pred(pos: CFGPosition): Set[Block] = {
    IntraProcBlockIRCursor.pred(pos) ++
    (pos match {
      case b: Block if b.isAfterCall =>
        b.incomingJumps.collect {
          _.parent.jump match { case d: DirectCall => d.target }
        }.map(_.returnBlock)
      case b: Block if b.isProcEntry => b.parent.incomingCalls().map(_.parent).toSet
      case _ => Set.empty 
    })
  }
}
object InterProcIRCursor extends InterProcIRCursor

trait CallGraph extends IRWalk[Procedure, Procedure] {
  final def succ(b: Procedure): Set[Procedure] = b.calls

  final def pred(b: Procedure): Set[Procedure] = b.incomingCalls().map(_.target).toSet
}

object CallGraph extends CallGraph

object InterProcBlockIRCursor extends InterProcBlockIRCursor

/** Computes the reachability transitive closure of the CFGPositions in initial under the successor relation defined by
  * walker.
  */
def computeDomain[T <: CFGPosition, O <: T](walker: IRWalk[T, O], initial: IterableOnce[O]): mutable.Set[O] = {
  val domain: mutable.Set[O] = mutable.Set.from(initial)

  var sizeBefore = 0
  var sizeAfter = domain.size
  while (sizeBefore != sizeAfter) {
    for (i <- domain) {
      domain.addAll(walker.succ(i))
      domain.addAll(walker.pred(i))
    }
    sizeBefore = sizeAfter
    sizeAfter = domain.size
  }
  domain
}

def toDot(program: Program, labels: Map[CFGPosition, String] = Map.empty): String = {
  val domain = computeDomain[CFGPosition, CFGPosition](IntraProcIRCursor, program.procedures)
  toDot[CFGPosition](domain, IntraProcIRCursor, labels)
}

def dotCallGraph(program: Program, labels: Map[CFGPosition, String] = Map.empty): String = {
  val domain = computeDomain[Procedure, Procedure](CallGraph, program.procedures)
  toDot[Procedure](domain, CallGraph, labels)
}

def dotBlockGraph(program: Program, labels: Map[CFGPosition, String] = Map.empty): String = {
  val domain = computeDomain[CFGPosition, Block](IntraProcBlockIRCursor, program.procedures.flatMap(_.blocks).toSet)
  toDot[Block](domain, IntraProcBlockIRCursor, labels)
}

def toDot[T <: CFGPosition](
    domain: mutable.Set[T],
    iterator: IRWalk[? >: T, ?],
    labels: Map[CFGPosition, String]
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
      case s: Block => f"[Block] ${s.label}"
      case s        => s.toString
    }
    if (labels.contains(node)) {
      text += "\n" ++ labels(node)
    }
    text
  }

  for (node <- domain) {
    node match
      case s: Command => dotNodes.addOne(s -> DotNode(label(s.label), nodeText(s)))
      case s: Block   => dotNodes.addOne(s -> DotNode(label(Some(s.label)), nodeText(s)))
      case s          => dotNodes.addOne(s -> DotNode(label(Some(s.toString)), nodeText(s)))
  }

  def getArrow(s: CFGPosition, n: CFGPosition) = {
    if (IRWalk.procedure(n) eq IRWalk.procedure(s)) {
      DotRegularArrow(dotNodes(s), dotNodes(n))
    } else {
      DotInterArrow(dotNodes(s), dotNodes(n))
    }
  }

  for (node <- domain) {
    iterator.succ(node).foreach(n => dotArrows.addOne(getArrow(node, n)))
 //       iterator.pred(s).foreach(n => dotArrows.addOne(getArrow(s,n)))
  }

  val allNodes = dotNodes.values.toList.sortBy(n => n.id)
  DotGraph("CursorCFG", allNodes, dotArrows).toDotString
}
