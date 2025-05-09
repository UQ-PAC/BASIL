package ir.slicer

import ir.*

class TransformCriterionPair(
  results: Map[CFGPosition, StatementSlice],
  slicingCriterion: Map[CFGPosition, StatementSlice]
) {

  private def result(n: CFGPosition) = results.getOrElse(n, StatementSlice())
  private def criterion(n: CFGPosition) = slicingCriterion.getOrElse(n, StatementSlice())

  def getPreCriterion(n: CFGPosition): StatementSlice = {
    def getPredecessorPair(parent: CFGPosition, pred: Option[CFGPosition]): StatementSlice = {
      pred match {
        case Some(p) => getPostCriterion(p)
        case None => getPreCriterion(parent)
      }
    }

    (n match {
      case p: Procedure => result(n)
      case b: Block => result(n)
      case a: LocalAssign => getPredecessorPair(a.parent, a.predecessor)
      case a: MemoryAssign => getPredecessorPair(a.parent, a.predecessor)
      case a: MemoryLoad => getPredecessorPair(a.parent, a.predecessor)
      case m: MemoryStore => getPredecessorPair(m.parent, m.predecessor)
      case a: Assume => getPredecessorPair(a.parent, a.predecessor)
      case a: Assert => getPredecessorPair(a.parent, a.predecessor)
      case c: Call => result(n)
      case n: NOP => result(n)
      case g: GoTo => result(n)
      case r: Return => getPredecessorPair(r.parent, r.parent.statements.lastOption)
      case u: Unreachable => result(n)
    }) ++ criterion(n)
  }

  def getPostCriterion(n: CFGPosition): StatementSlice = {
    n match {
      case p: Procedure => {
        p.returnBlock match {
          case Some(block) => getPostCriterion(block)
          case None => p.incomingCalls().flatMap(result(_)).toSet
        }
      }
      case b: Block => {
        b.nextBlocks match {
          case Nil => getPostCriterion(b.jump)
          case blocks => blocks.flatMap(result(_)).toSet
        }
      }
      case a: LocalAssign => result(n)
      case a: MemoryAssign => result(n)
      case a: MemoryLoad => result(n)
      case a: MemoryStore => result(n)
      case a: Assume => result(n)
      case a: Assert => result(n)
      case c: Call => getPreCriterion(c.successor)
      case n: NOP => result(n)
      case g: GoTo => result(n)
      case r: Return => result(n)
      case u: Unreachable => result(n)
    }
  }
}
