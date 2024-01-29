package ir
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

// todo: we could just use the dependencies trait directly instead to avoid the instantiation issue
trait IRWalk[IN <: CFGPosition, NT <: CFGPosition & IN] {
  def succ(pos: IN): Set[NT]
  def pred(pos: IN): Set[NT]
}

trait IntraProcIRCursor extends IRWalk[CFGPosition, CFGPosition] {

  def succ(pos: CFGPosition): Set[CFGPosition] = {
    pos match {
      case proc: Procedure => proc.entryBlock.toSet
      case b: Block        => Set(b.statements.headOption().getOrElse(b.jump))
      case s: Statement    => Set(s.parent.statements.nextOption(s).getOrElse(s.parent.jump))
      case j: Jump =>
        j match {
          case n: GoTo => n.targets.asInstanceOf[Set[CFGPosition]]
          case c: DirectCall =>
            c.returnTarget match
              case Some(b) => Set(b)
              case None    => Set()
          case i: IndirectCall =>
            i.returnTarget match
              case Some(block: Block) => Set[CFGPosition](block)
              case None               => Set()
        }
    }
  }

  def pred(pos: CFGPosition): Set[CFGPosition] = {
    pos match {
      case s: Statement =>
        if (s.parent.statements.hasPrev(s)) {
          Set(s.parent.statements.getPrev(s))
        } else {
          Set(s.parent) // predecessor blocks
        }
      case j: Jump         => if j.parent.statements.isEmpty then Set(j.parent) else Set(j.parent.statements.last)
      case b: Block        => b.incomingJumps.asInstanceOf[Set[CFGPosition]]
      case proc: Procedure => Set() // intraproc
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
      case s: Procedure => s.entryBlock.toSet
    }
  }

  @tailrec
  final def pred(pos: CFGPosition): Set[Block] = {
    pos match {
      case b: Block     => if b.isEntry then Set.empty else b.incomingJumps.map(_.parent)
      case j: Command   => pred(j.parent)
      case s: Procedure => Set.empty
    }
  }
}
object IntraProcBlockIRCursor extends IntraProcBlockIRCursor

trait InterProcIRCursor extends IRWalk[CFGPosition, CFGPosition] {

  final def succ(pos: CFGPosition): Set[CFGPosition] = {
    pos match
      case c: DirectCall   => Set(c.target)
      case c: IndirectCall => Set()
      case _               => IntraProcIRCursor.succ(pos)
  }
  final def pred(pos: CFGPosition): Set[CFGPosition] = {
    IntraProcIRCursor.pred(pos) ++ (pos match
      case c: Procedure => c.incomingCalls().toSet.asInstanceOf[Set[CFGPosition]]
      case b: Block     => if b.isEntry then Set(b.parent) else b.incomingJumps.asInstanceOf[Set[CFGPosition]]
      case _            => Set()
    )
  }
}

trait InterProcBlockIRCursor extends IRWalk[CFGPosition, Block] {

  final def succ(pos: CFGPosition): Set[Block] = {
    pos match {
      case s: DirectCall => s.target.entryBlock.toSet
      case _             => IntraProcBlockIRCursor.succ(pos)
    }
  }

  final def pred(pos: CFGPosition): Set[Block] = {
    pos match {
      case b: Block => if b.isEntry then b.parent.incomingCalls().map(_.parent).toSet else b.prevBlocks.toSet
      case _        => IntraProcBlockIRCursor.pred(pos)
    }
  }
}
object InterProcIRCursor extends InterProcIRCursor

trait CallGraph extends IRWalk[Procedure, Procedure] {
  final def succ(b: Procedure): Set[Procedure] = b.calls

  final def pred(b: Procedure): Set[Procedure] = b.incomingCalls().map(_.target).toSet
}

object CallGraph extends CallGraph

object InterProcBlockIRCursor extends IntraProcBlockIRCursor

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
  var dotArrows = mutable.ListBuffer[DotArrow]()

  def nodeText(node: CFGPosition): String = {
    var text = node match {
      case s: Block => f"[Block] ${s.label}"
      case s        => s.toString
    }
    if (labels.contains(node)) {
      text = labels(node) + "\n" + text // TODO
    }
    text
  }

  for (node <- domain) {
    node match
      case s: Command => dotNodes.addOne(s -> DotNode(label(s.label), nodeText(s)))
      case s: Block   => dotNodes.addOne(s -> DotNode(label(Some(s.label)), nodeText(s)))
      case s          => dotNodes.addOne(s -> DotNode(label(Some(s.toString)), nodeText(s)))
  }

  for (node <- domain) {
    node match {
      case s: Call =>
        iterator.succ(s).foreach(n => dotArrows.addOne(DotInterArrow(dotNodes(s), dotNodes(n))))
      case s =>
        iterator.succ(s).foreach(n => dotArrows.addOne(DotRegularArrow(dotNodes(s), dotNodes(n))))
    }
  }

  val allNodes = dotNodes.values.toList.sortBy(n => n.id)
  new DotGraph("CursorCFG", allNodes, dotArrows).toDotString
}
