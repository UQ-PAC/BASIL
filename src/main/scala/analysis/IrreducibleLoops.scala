package analysis

import ir.{CFGPosition, Command, IntraProcIRCursor, Program, Procedure, Block, GoTo, IRWalk}
import util.intrusive_list.IntrusiveList
import util.Logger

import scala.collection.mutable

private def label(p: CFGPosition) = {
  p match {
    case b: Block => "block." + b.label
    case p: Procedure => "proc." + p.name
    case c: Command => "cmd." + c.hashCode().toString
  }
}

/*
 * Loop Identification
 *
 * 
 */

/* A connection between to IL nodes, purely for the representation of loops in
 *
 */
case class LoopEdge(from: CFGPosition, to: CFGPosition) {
  override def toString: String = s"(${label(from)}, ${label(to)})"
}

/* A loop is a subgraph <G_l, E_l> of a CFG <G, E>
 *
 */
class Loop(val header: CFGPosition) {
  val reentries: mutable.Set[LoopEdge] = mutable.Set() // Edges to loop from outside that are not to the header
  val backEdges: mutable.Set[LoopEdge] = mutable.Set() // Edges from inside loop to the header
  val entryEdges: mutable.Set[LoopEdge] = mutable.Set() // Edges into the header node

  val nodes: mutable.Set[CFGPosition] = mutable.Set() // G_l
  val edges: mutable.Set[LoopEdge] = mutable.Set() // G_e
  var reducible: Boolean = true // Assume reducible by default

  def addEdge(edge: LoopEdge): Unit = {
    nodes += edge.from
    nodes += edge.to
    edges += edge
  }

  def name: String = label(header)

  override def toString: String = {
    s"Header: ${label(header)}, Body: $edges"
  }
}

/* Loop detection and classification with respect to being reducible or irreducible. Implements the algorithm
 *  described by Wei in `A New Algorithm for Identifying Loops in Decompilation` (LNCS 4632 pp 170-183)
 */
class LoopDetector(cfg: Program) {
  // Header -> Loop
  private val loops: mutable.HashMap[CFGPosition, Loop] = mutable.HashMap()
  private val headers: mutable.Set[CFGPosition] = mutable.Set()

  // Algorithm helpers
  private val visitedNodes: mutable.Set[CFGPosition] = mutable.Set()
  private val nodeDFSPpos: mutable.HashMap[CFGPosition, Int] = mutable.HashMap()
  private val iloopHeaders: mutable.HashMap[CFGPosition, CFGPosition] = mutable.HashMap()
  private val edgeStack: mutable.Stack[LoopEdge] = mutable.Stack()

  /*
   * Returns the set of irreducible loops in the program.
   *
   */
  def irreducible_loops(): Set[Loop] = {
    val irreducibleLoops: Set[Loop] = loops.values.filter(l => !l.reducible).toSet
    irreducibleLoops

    /*
    TODO is this supposed to be anything?
    if (irreducibleLoops.isEmpty) {
      irreducibleLoops
    } else {
      val wantedLoops: mutable.Set[Loop] = mutable.Set[Loop]()
      val irrHeaders: Set[CFGPosition] = irreducibleLoops.map(l => l.header)

      irreducibleLoops
    }
    */
  }

  /*
   * Returns the set of loops in the program.
   */
  def identify_loops(): Set[Loop] = {
    val funcEntries = cfg.procedures
    funcEntries.foreach { funcEntry => traverse_loops_dfs(funcEntry, 1) }
    loops.values.toSet
  }

  /*
   * This algorithm performs a DFS on the CFG, starting from the node `h0` (the initial one passed)
   *  and tags nodes as visited as it goes. There are 5 possible pathways on visit for each node `b`:
   *   (a) `b` has not been visited before.
   *   (b) `b` has been visited and is in the current path, i.e., it is a new loop header.
   *   (c) `b` has been visited, though is not in the current path, and is not part of a loop. It is
   *          thus regular control flow unrelated to the headers.
   *   (d) `b` has been visited, though is not in the current path. It is part of a loop with a header
   *          in our path though, so it is another node within our loop.
   *   (e) `b` has been visited, though is not part of the current path. It is part of a loop, though
   *          not any loop that our current loop context is a part of - thus this node must be a re-entry
   *          node into the target loop, making it irreducible, so we mark it as such.
   *
   */
  private def traverse_loops_dfs(b0: CFGPosition, DFSPpos: Int): Option[CFGPosition] = {

    visitedNodes += b0
    nodeDFSPpos(b0) = DFSPpos

    // Process all outgoing edges from the current node
    // IntraProcIRCursor.succ(b0).toList.sortBy(n => n.to.ed).foreach{ toNode =>
    //  The above makes the iteration of loops deterministic. The algorithm (and transform) should be agnostic of how loops are identified
    //      (in the sense its application should completely resolve any irreducibility), though by the nature of irreducible loops they can be
    //      characterised in different ways
    IntraProcIRCursor.succ(b0).foreach { toNode =>
      val edge = LoopEdge(b0, toNode)
      edgeStack.push(edge)
      val from = edge.from
      val b = edge.to

      if (!visitedNodes.contains(b)) {
        // Case (a)
        val nh: Option[CFGPosition] = traverse_loops_dfs(b, DFSPpos + 1)
        nh.foreach(b => tag_lhead(b0, b))
      } else {
        if (nodeDFSPpos(b) > 0) {
          // Case (b)
          // b is in DFSP(b0)
          headers += b

          val newLoop = if (loops.contains(b)) loops(b) else Loop(b)
          edgeStack.reverse.slice(nodeDFSPpos(b) - 1, nodeDFSPpos(b0)).foreach { pEdge => newLoop.addEdge(pEdge) }
          newLoop.backEdges += edge

          // Add loop entry edge
          IntraProcIRCursor.pred(b).foreach { predNode =>
            val predEdge = LoopEdge(predNode, b)
            if (nodeDFSPpos.contains(predNode)) {
              if (nodeDFSPpos(predNode) > 0 && predEdge != edge) {
                newLoop.entryEdges += predEdge
              }
            }
          }

          if (!loops.contains(b)) {
            loops(b) = newLoop
          }

          tag_lhead(b0, b)
        } else if (!iloopHeaders.contains(b)) {
          // Case (c) - do nothing
          // b is not part of this path, and it's not a header, so we can ignore it.
          // (in effect we add it to our path later, but we do that when we discover a new header
          //  by looking through the instruction call stack)
        } else {
          var h: CFGPosition = iloopHeaders(b)
          if (nodeDFSPpos(h) > 0) {
            // Case (d)
            // h is in DFSP(b0)
            val loop = loops(h)

            // Add current path to the existing loop (a new path in the loop is discovered)
            // This can happen for example in the case that there is a branch in a loop, or a `continue` stmt, etc
            edgeStack.reverse.slice(nodeDFSPpos(h) - 1, nodeDFSPpos(b0)).foreach {
              pEdge => loop.addEdge(pEdge)
            }
            IntraProcIRCursor.succ(b).filter(n => n == h).foreach { n =>
              val outEdge = LoopEdge(b, n)
              loop.addEdge(outEdge)
            }

            tag_lhead(b0, h)
          } else {
            // Case (e)
            // reentry (irreducible!)
            val loop = loops(h)
            loop.reducible = false
            loop.reentries += edge

            // Make outer loops irreducible if the originating node of the re-entry edge is not within those loops
            IntraProcIRCursor.succ(b).foreach { nextNode =>
              val nextEdge = LoopEdge(b, nextNode)
              var ih = nextNode

              while (iloopHeaders.contains(ih)) {
                ih = iloopHeaders(ih)
                val thisLoop = loops(ih)
                if (iloopHeaders.contains(from)) {
                  if (iloopHeaders(from) != ih) {
                    thisLoop.reducible = false
                    thisLoop.reentries += edge
                  }
                } else {
                  // `from` must be outside the loop
                  thisLoop.reducible = false
                  thisLoop.reentries += edge
                }
              }
            }

            var break = false
            while (iloopHeaders.contains(h) && !break) {
              h = iloopHeaders(h)
              if (nodeDFSPpos(h) > 0) {
                tag_lhead(b0, h)
                break = true
              }
            }
          }
        }
      }
      edgeStack.pop() // Here the most recent edge will be originating from `b0`
    }
    nodeDFSPpos(b0) = 0
    iloopHeaders.get(b0)
  }

  /** Sets the most inner loop header `h` for a given node `b`
    *
    */
  private def tag_lhead(b: CFGPosition, h: CFGPosition): Unit = {
    var cur1: CFGPosition = b
    var cur2: CFGPosition = h

    if (cur1 == cur2) {
      return // We don't consider self-loops to be loops for our purposes
    }

    var hasLoopHeader: Boolean = iloopHeaders.contains(cur1)
    while (hasLoopHeader) {
      val ih = iloopHeaders(cur1)
      if (ih == cur2) {
        return
      }
      if (nodeDFSPpos(ih) < nodeDFSPpos(cur2)) {
        iloopHeaders(cur1) = cur2
        cur1 = cur2
        cur2 = ih
      } else {
        cur1 = ih
      }
      hasLoopHeader = iloopHeaders.contains(cur1)
    }
    iloopHeaders(cur1) = cur2
  }
}


class LoopTransform(loops: Set[Loop]) {

  /* For every irreducible loop, transform it into a reducible one. The algorithm
   *  for doing this is "inspired" (read: copied) from LLVM, the source for which
   *  can be found here: https://llvm.org/doxygen/FixIrreducible_8cpp_source.html
   *
   * Returns: Set of loops which were previously irreducible, but are no longer
   */
  def llvm_transform(): Set[Loop] = {
    val newLoops: mutable.Set[Loop] = mutable.Set()
    var i: Int = 0 // Exclusively for tracking
    loops.filter(l => !l.reducible).foreach { l =>
      newLoops += llvm_transform_loop(l, i)
      i = i + 1
    }

    newLoops.toSet
  }

  /* Performs the LLVM transform for an individual loop. The algorithm is as follows:
   *
   * 1. Collect the set of headers, `H`, of the SCC
   *     - note here that headers of an SCC include re-entry nodes, which distinguishes it from a loop. In
   *         all other regards however this is equivalent to a loop.
   * 2. Collect the set of predecessors, `P`, of these headers. These can be inside or outside the SCC
   * 3. Create block `N` and redirect every edge from set `P` to `H` via `N`
   *
   * Returns: A new reducible loop which is semantically equivalent to the input irreducible loop
   */
  private def llvm_transform_loop(loop: Loop, i: Int): Loop = {
    val entryEdges: Set[LoopEdge] = loop.entryEdges.union(loop.reentries).union(loop.backEdges).toSet

    val P_e: Set[LoopEdge] = entryEdges // N entry edges
    val P_b: Set[LoopEdge] = entryEdges.flatMap { e =>
      val predNodes = IntraProcIRCursor.pred(e.to)
      predNodes.map(a => LoopEdge(a, e.to))
    } -- P_e // N back edges
    val body: Set[LoopEdge] = loop.edges.toSet -- P_b // Regular control flow in the loop


    // 3. Create block `N` and redirect every edge from set `P` to `H` via `N`
    val conns = P_e.map(e => e.to).union(P_b.map(e => e.to)).collect { case blk: Block => blk }
    val NGoTo = GoTo(conns)

    val N = Block(s"${IRWalk.procedure(loop.header).name}_N_$i", jump = NGoTo)
    IRWalk.procedure(loop.header).addBlock(N)

    val newLoop = Loop(N)
    newLoop.edges ++= body

    P_e.foreach { originalEdge =>
      val origNode = originalEdge.from
      val origDest = originalEdge.to

      origNode match {
        case origBlock: Block =>
          origBlock.replaceJump(GoTo(List(N)))
          newLoop.addEdge(LoopEdge(origBlock, N))
          newLoop.addEdge(LoopEdge(N, origDest))

        case goto: GoTo =>
          origDest match {
            case origDest: Block =>
              goto.removeTarget(origDest)
              goto.addTarget(N)

              newLoop.addEdge(LoopEdge(goto, N))
              newLoop.addEdge(LoopEdge(N, origDest))
            case _ =>
          }
        case _ =>
          Logger.error("Unexpected loop originating node - 1")
          Logger.error(origNode)
      }
    }

    P_b.foreach { originalEdge =>
      val origNode = originalEdge.from
      val origDest = originalEdge.to

      origNode match {
        case origBlock: Block =>
          origBlock.replaceJump(GoTo(List(N)))
          val toEdge = LoopEdge(origBlock, N)

          newLoop.addEdge(toEdge)
          newLoop.addEdge(LoopEdge(N, origDest))
          newLoop.backEdges += toEdge
        case goto: GoTo =>
          origDest match {
            case origDest: Block =>
              goto.removeTarget(origDest)
              goto.addTarget(N)

              newLoop.addEdge(LoopEdge(goto, N))
              newLoop.addEdge(LoopEdge(N, origDest))
            case _ =>
          }
        case _ =>
          Logger.error("Unexpected loop originating node - 1")
          Logger.error(origNode)
      }
    }

    newLoop
  }
}
