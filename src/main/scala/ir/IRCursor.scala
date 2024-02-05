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

// todo: we could just use the dependencies trait directly instead to avoid the instantiation issue
trait IRWalk[IN <: CFGPosition, NT <: CFGPosition & IN] {
  def succ(pos: IN): Set[NT]
  def pred(pos: IN): Set[NT]
}

object IRWalk:
  def procedure(pos: CFGPosition) : Procedure = {
    Logger.info(s"pos: $pos")
    pos match {
      case p: Procedure => p
      case b: Block => b.parent
      case c: Command => c.parent.parent
    }
  }

  def blockBegin(pos: CFGPosition) : Option[Block] = {
    pos match {
      case p: Procedure => p.entryBlock
      case b: Block => Some(b)
      case c: Command => Some(c.parent)
    }
  }

  def commandBegin(pos: CFGPosition) : Option[Command] = {
    pos match {
      case p: Procedure => p.entryBlock.map(b => b.statements.headOption().getOrElse(b.jump))
      case b: Block => Some(b.statements.headOption().getOrElse(b.jump))
      case c: Command => Some(c)
    }
  }


/**
 * Does not include edges between procedures.
 */
trait IntraProcIRCursor extends IRWalk[CFGPosition, CFGPosition] {

  def succ(pos: CFGPosition): Set[CFGPosition] = {
    pos match {
      case proc: Procedure => proc.entryBlock.toSet
      case b: Block        => Set(b.statements.headOption().getOrElse(b.jump))
      case s: Statement    =>  Set(s.succ().getOrElse(s.parent.jump))
      case j: Jump =>
        j match {
          case n: GoTo         => n.targets.asInstanceOf[Set[CFGPosition]]
          case c: DirectCall   => c.afterCall.toSet
          case i: IndirectCall => i.afterCall.toSet
        }
    }
  }

  def pred(pos: CFGPosition): Set[CFGPosition] = {
    pos match {
      case s: Statement => Set(s.pred().getOrElse(s.parent))
      case j: Jump => Set(j.parent.statements.lastOption.getOrElse(j.parent))
      case b: Block =>
        b.kind match {
          case AfterCall(from) => Set(from)
          case Entry(proc)      => Set(proc)
          case _                => b.incomingJumps.asInstanceOf[Set[CFGPosition]]
        }
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
      case b: Block =>
        b.kind match {
          case Entry(_)         => Set.empty
          case AfterCall(from) => Set(from.parent)
          case _                => b.incomingJumps.map(_.parent)
        }
      case j: Command   => pred(j.parent)
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
    pos match
      case c: DirectCall   => Set(c.target)
      case c: IndirectCall =>  c.parent.kind match
        case Return(proc) =>
          proc.incomingCalls().flatMap(_.afterCall).toSet
        case _ => Set()
      case _ => IntraProcIRCursor.succ(pos)
  }

  final def pred(pos: CFGPosition): Set[CFGPosition] = {
    pos match
      case c: Procedure       => c.incomingCalls().toSet.asInstanceOf[Set[CFGPosition]]
      case b: Block =>
        b.kind match {
          case AfterCall(DirectCall(target, afterCall, _)) => target.returnBlock.map(_.jump).toSet
          case _ => IntraProcIRCursor.pred(pos)
        }
      case _ => IntraProcIRCursor.pred(pos)
  }
}

trait InterProcBlockIRCursor extends IRWalk[CFGPosition, Block] {

  final def succ(pos: CFGPosition): Set[Block] = {
    pos match {
      case s: DirectCall   => s.target.entryBlock.toSet
      case s: IndirectCall => Set()
      case _               => IntraProcBlockIRCursor.succ(pos)
    }
  }

  final def pred(pos: CFGPosition): Set[Block] = {
    pos match {
      case b: Block =>
        b.kind match {
          case AfterCall(from) => from.parent.parent.returnBlock.toSet
          case Entry(of)        => of.incomingCalls().map(_.parent).toSet
          case _                => b.prevBlocks.toSet
        }
      case _ => IntraProcBlockIRCursor.pred(pos)
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
      DotRegularArrow(dotNodes(s),dotNodes(n))
    } else {
      DotInterArrow(dotNodes(s),dotNodes(n))
    }
  }

  for (node <- domain) {
    node match {
      case s =>
        iterator.succ(s).foreach(n => dotArrows.addOne(getArrow(s,n)))
 //       iterator.pred(s).foreach(n => dotArrows.addOne(getArrow(s,n)))
    }
  }

  val allNodes = dotNodes.values.toList.sortBy(n => n.id)
  new DotGraph("CursorCFG", allNodes, dotArrows).toDotString
}
