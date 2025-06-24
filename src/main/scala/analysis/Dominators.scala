package analysis

/**
 * This implements an intra-procedural dominator algorithm.
 *
 * A block b1 "dominates" another block b2 iff every path
 * from the entry to b2 passes through b1.
 *
 */

import ir._

import scala.annotation.tailrec
import scala.collection.immutable
import scala.collection.mutable

/**
 * Dominator analysis using the "improved iterative" algorithm (figure 3)
 * presented in:
 *
 *     Cooper, Keith D., Harvey, Timothy J. and Kennedy, Ken.
 *     "A simple, fast dominance algorithm." (2006) https://hdl.handle.net/1911/96345.
 *
 */
object Dominators {

  /** Internal-use mutable map of dominators. */
  protected type Doms = mutable.Map[Block, Node]

  type Node = Undefined.type | Block
  case object Undefined

  /**
   * The result of the dominator analysis is a map representing
   * the dominator *tree*. Each block is mapped to its
   * unique *immediate* dominator - that is, the dominator
   * which dominates all other dominators.
   *
   * A block's value may be the special Undefined value if it
   * is unreachable from the start block.
   */
  type Result = immutable.Map[Block, Node]

  /**
   * Computes the dominator tree for the given procedure.
   * The given procedure must have at least an entry block.
   */
  def computeDominatorTree(p: Procedure): Result = {
    val entry = p.entryBlock.getOrElse(throw Exception("attempt to compute dominators with no entry block"))

    ir.transforms.reversePostOrder(p)

    val doms: Doms = mutable.Map.from(p.blocks.map(_ -> Undefined))
    doms(entry) = entry

    val allBlocksRPO = Array.from(p.blocks).sortBy(_.rpoOrder)

    var changed = true
    while (changed) {
      // println("iteration")
      changed = false
      for (b <- allBlocksRPO) {
        if (b != entry) {
          val new_idom = b.prevBlocks
            .to(mutable.ArraySeq)
            .sortBy(_.rpoOrder)
            .foldLeft(Undefined: Node)((a, b) => intersect(doms, a, b))

          val old = doms.put(b, new_idom)
          if (old != Some(new_idom)) {
            changed = true
          }
        }

      }
    }

    doms.toMap
  }

  /**
   * Returns a function which to determine whether block b1 dominates block b2,
   * given a pre-computed dominator tree.
   */
  def dominates(doms: Result | Doms): (Block, Block) => Boolean = {

    @tailrec
    def go(b1: Block, b2: Block): Boolean = {
      if (b1 == b2) then {
        true
      } else {
        doms(b2) match {
          // NOTE: should every block dominate an unreachable block? currently, we say
          // nothing dominates an unreachable block.
          case Undefined => false
          case nextb2: Block if nextb2 != b2 => go(b1, nextb2)
          case _: Block => false
        }
      }
    }

    go
  }

  def nodeToString(x: Node) =
    x match {
      case b: Block => b.label
      case _ => x.toString
    }

  def printDoms(doms: Doms | Result) = {
    doms.foreach((k, v) => {
      println(k.label + " -> " + nodeToString(v))
    })
  }

  /**
   * Computes a kind of set intersection of the dominators of the given blocks.
   */
  protected def intersect(doms: Doms, b1: Node, b2: Block): Node = {
    b1 match {
      // NOTE: the paper explicitly filters out Undefined values.
      // but we keep them and handle them here in the intersect function.
      // if b2 is not yet processed, then ignore it.
      case _ if doms(b2) == Undefined => b1

      // this will only be encountered in the first iteration, as
      // Undefined is the base case for the fold and all other fold elements
      // are blocks.
      case Undefined => b2

      // the following is the intersect algorithm from the paper.
      case b1: Block => {
        var finger1 = b1
        var finger2 = b2
        while (finger1 != finger2) {
          // paper uses "finger1 < finger2", with an implicit numbering which is RPO???
          while (finger1.rpoOrder < finger2.rpoOrder) {
            // casting should be guaranteed by rpoorder and ensuring that
            // b2 has been processed.
            finger1 = doms(finger1).asInstanceOf[Block]
          }
          while (finger2.rpoOrder < finger1.rpoOrder) {
            finger2 = doms(finger2).asInstanceOf[Block]
          }
        }
        finger1
      }
    }
  }
}
