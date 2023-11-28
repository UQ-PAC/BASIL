package ir
import cfg_visualiser.DotElement
import cfg_visualiser.{DotArrow, DotGraph, DotInlineArrow, DotInterArrow, DotIntraArrow, DotNode, DotRegularArrow}

import collection.mutable
import scala.annotation.tailrec

/*
 * Defines a position in the IL / CFG; this becomes the lhs of the state map lattice in a static analysis.
 */
type CFGPosition = Procedure | Block | Command


// Interprocedural
//  position = (call string) + Position



/*
    Closed trace-perspective beginning of a composite IL structure.
 */
def begin(c: CFGPosition): CFGPosition = {
  c match {
    case p:Procedure => p
    case b:Block => b
    case s:Statement => s.parent.statements.head()
    case s:Jump => s
  }
}

/*
    Closed trace-perspective end of a composite IL structure.
 */
@tailrec
def end(c: CFGPosition): CFGPosition = {
  c match {
    case p:Procedure => end(p.returnBlock)
    case b:Block => b.jump
    case s:Statement => s.parent.statements.back()
    case s:Jump => s
  }
}


object IntraProcIRCursor {
  type Node = CFGPosition

  def succ(pos: CFGPosition): Set[CFGPosition] = {
    pos match {
      case proc: Procedure =>
        if proc.entryBlock.isEmpty then Set(proc.returnBlock) else Set(proc.entryBlock.get)
      case b: Block =>
        if b.statements.isEmpty
        then Set.from(b.jumpSet)
        else Set[CFGPosition](b.statements.head())
      case s: Statement =>
        if (s.parent.statements.hasNext(s)) {
          Set(s.parent.statements.getNext(s))
        } else {
          Set(s.parent.jump)
        }
      case j: Jump => j match {
        case n: GoTo => n.targets.asInstanceOf[Set[CFGPosition]]
        case c: DirectCall => c.returnTarget match
          case Some(b) => Set(b)
          case None => Set()
        case i: IndirectCall =>
          if (i.target.name == "R30") {
            Set()
          } else {
            i.returnTarget match
              case Some(block: Block) => Set[CFGPosition](block)
              case None => Set()
          }
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
      case j: Jump => if j.parent.statements.isEmpty then Set(j.parent) else Set(j.parent.statements.last)
      case b: Block => b.predecessors.map(end)
      case proc: Procedure => Set() // intraproc
    }
  }
}

object InterProcIRCursor {
  type Node = CFGPosition

  def succ(pos: CFGPosition): Set[CFGPosition] = {
    IntraProcIRCursor.succ(pos) ++ (pos match
      case c: DirectCall => Set(c.target)
      case _ => Set()
      )
  }
    def pred(pos: CFGPosition): Set[CFGPosition] = {
      IntraProcIRCursor.pred(pos) ++ (pos match
        case c: Procedure => c.callers().map(end)
        case b: Block => if b.isReturn then b.parent.callers().map(end) else Set()
        case _ => Set()
        )
    }
}


def computeDomain(prog: Program): mutable.Set[CFGPosition] = {
  val domain : mutable.Set[CFGPosition] = mutable.Set.from(prog.procedures)

  var sizeBefore = 0
  var sizeAfter = domain.size
  while (sizeBefore != sizeAfter) {
    for (i <- domain) {
      domain.addAll(IntraProcIRCursor.succ(i))
      domain.addAll(IntraProcIRCursor.pred(i))
    }
    sizeBefore = sizeAfter
    sizeAfter = domain.size
  }
  domain
} 


def toDot(prog: Program, labels: Map[CFGPosition, String] = Map.empty) : String = {
  val visited : mutable.Set[CFGPosition] = mutable.Set.from(prog.procedures)
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

  val domain = computeDomain(prog)

  def nodeText(node: CFGPosition): String = {
    var text = node match {
      case s: Block => f"[Block] ${s.label}"
      case s => s.toString
    }
    if (labels.contains(node)) {
      text += "\n" ++ labels(node)
    }
    text
  }

  for (node <- domain) {
    node match
      case s: Command => dotNodes.addOne(s -> DotNode(label(s.label), nodeText(s)))
      case s: Block => dotNodes.addOne(s -> DotNode(label(Some(s.label)), nodeText(s)))
      case s => dotNodes.addOne(s -> DotNode(label(Some(s.toString)), nodeText(s)))
  }

  for (node <- domain) {
    node match {
      case s : Call =>
        IntraProcIRCursor.succ(s).foreach(n => dotArrows.addOne(DotInterArrow(dotNodes(s), dotNodes(n))))
      case s =>
        IntraProcIRCursor.succ(s).foreach(n => dotArrows.addOne(DotRegularArrow(dotNodes(s), dotNodes(n))))
      case _ => ()
    }
  }

  val allNodes = dotNodes.values.toList.sortBy(n => n.id)
  new DotGraph("CursorCFG", allNodes, dotArrows).toDotString
}