package analysis

import scala.annotation.tailrec
import ir.{Block, Command, IntraProcIRCursor, Program, Procedure, GoTo, IRWalk}
import util.intrusive_list.IntrusiveList
import util.Logger

import scala.collection.mutable

private def label(p: Block) = "block." + p.label

/*
 * Loop Identification
 *
 * 
 */

/* A connection between to IL nodes, purely for the representation of loops in
 *
 */
case class LoopEdge(from: Block, to: Block) {
  override def toString: String = s"(${label(from)}, ${label(to)})"
}

/* A loop is a subgraph <G_l, E_l> of a CFG <G, E>
 *
 */
class Loop(val header: Block) {
  val reentries: mutable.Set[LoopEdge] = mutable.Set() // Edges to loop from outside that are not to the header
  val backEdges: mutable.Set[LoopEdge] = mutable.Set() // Edges from inside loop to the header
  val entryEdges: mutable.Set[LoopEdge] = mutable.Set() // Edges into the header node

  val nodes: mutable.Set[Block] = mutable.Set() // G_l
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
object LoopDetector {

  case class State(
    // Header -> Loop
    loops: Map[Block, Loop] = Map(),
    headers: Set[Block] = Set(),

    // Algorithm helpers
    visitedNodes: Set[Block] = Set(),
    nodeDFSPpos: Map[Block, Int] = Map(),
    iloopHeaders: Map[Block, Block] = Map(),
    edgeStack: List[LoopEdge] = List(),

  ) {
    def irreducibleLoops = loops.values.filter(l => !l.reducible).toSet

    def identifiedLoops = loops.values

    def reducibleTransformIR() : State = {
      this.copy(loops = LoopTransform.llvm_transform(loops.values).map(l => l.header -> l).toMap)
    }
  }

  def identify_loops(entryBlock: Block): State = {
    traverse_loops_dfs(State(), entryBlock, 1)._1
  }

  /*
   * Returns the set of loops for each procedure in the program.
   */
  def identify_loops(cfg: Program): State = {
    cfg.procedures.toSet.map(_.entryBlock).flatten
      .foldLeft(State())((st, eb) => (traverse_loops_dfs(st, eb, 1)._1))
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
  private def traverse_loops_dfs(istate: State, b0: Block, DFSPpos: Int): (State, Option[Block]) = {
    var st = istate

    st = st.copy(visitedNodes = st.visitedNodes + b0)
    st = st.copy(nodeDFSPpos = st.nodeDFSPpos.updated(b0, DFSPpos))

    // Process all outgoing edges from the current node
    // IntraProcIRCursor.succ(b0).toList.sortBy(n => n.to.ed).foreach{ toNode =>
    //  The above makes the iteration of loops deterministic. The algorithm (and transform) should be agnostic of how loops are identified
    //      (in the sense its application should completely resolve any irreducibility), though by the nature of irreducible loops they can be
    //      characterised in different ways
    b0.nextBlocks.foreach { toNode =>
      val edge = LoopEdge(b0, toNode)
      st = st.copy(edgeStack = edge::st.edgeStack)
      val from = edge.from
      val b = edge.to

      if (!st.visitedNodes.contains(b)) {
        // Case (a)
        val (x,y) = traverse_loops_dfs(st, b, DFSPpos + 1)
        st = x
        val nh: Option[Block] = y
        st = nh.foldLeft(st)((st, b) => tag_lhead(st, b0, b))
      } else {
        if (st.nodeDFSPpos(b) > 0) {
          // Case (b)
          // b is in DFSP(b0)
          st = st.copy(headers = st.headers + b)

          val newLoop = st.loops.get(b).getOrElse(Loop(b))
          // TODO: is this correct with current stack
          st.edgeStack.reverse.slice(st.nodeDFSPpos(b) - 1, st.nodeDFSPpos(b0)).foreach { pEdge => newLoop.addEdge(pEdge) }
          newLoop.backEdges += edge

          // Add loop entry edge
          b.prevBlocks.foreach { predNode =>
            val predEdge = LoopEdge(predNode, b)
            if (st.nodeDFSPpos.contains(predNode)) {
              if (st.nodeDFSPpos(predNode) > 0 && predEdge != edge) {
                newLoop.entryEdges += predEdge
              }
            }
          }

          if (!st.loops.contains(b)) {
            st = st.copy(loops = st.loops.updated(b, newLoop))
          }

          st = tag_lhead(st, b0, b)
        } else if (!st.iloopHeaders.contains(b)) {
          // Case (c) - do nothing
          // b is not part of this path, and it's not a header, so we can ignore it.
          // (in effect we add it to our path later, but we do that when we discover a new header
          //  by looking through the instruction call stack)
        } else {
          var h: Block = st.iloopHeaders(b)
          if (st.nodeDFSPpos(h) > 0) {
            // Case (d)
            // h is in DFSP(b0)
            val loop = st.loops(h)

            // Add current path to the existing loop (a new path in the loop is discovered)
            // This can happen for example in the case that there is a branch in a loop, or a `continue` stmt, etc
            st.edgeStack.reverse.slice(st.nodeDFSPpos(h) - 1, st.nodeDFSPpos(b0)).foreach(loop.addEdge)
            b.nextBlocks.filter(n => n == h).foreach { n =>
              val outEdge = LoopEdge(b, n)
              loop.addEdge(outEdge)
            }

            st = tag_lhead(st, b0, h)
          } else {
            // Case (e)
            // reentry (irreducible!)
            val loop = st.loops(h)
            loop.reducible = false
            loop.reentries += edge

            // Make outer loops irreducible if the originating node of the re-entry edge is not within those loops
            b.nextBlocks.foreach { nextNode =>
              val nextEdge = LoopEdge(b, nextNode)
              var ih = nextNode

              while (st.iloopHeaders.contains(ih)) {
                ih = st.iloopHeaders(ih)
                val thisLoop = st.loops(ih)
                if (st.iloopHeaders.contains(from)) {
                  if (st.iloopHeaders(from) != ih) {
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
            while (st.iloopHeaders.contains(h) && !break) {
              h = st.iloopHeaders(h)
              if (st.nodeDFSPpos(h) > 0) {
                st = tag_lhead(st, b0, h)
                break = true
              }
            }
          }
        }
      }
      // Here the most recent edge will be originating from `b0`
      st = st.copy(edgeStack = st.edgeStack match {
        case h :: tl => tl
        case Nil => Nil
      })

    }
    st = st.copy(nodeDFSPpos = st.nodeDFSPpos.updated(b0, 0))
    (st, st.iloopHeaders.get(b0))
  }

  /** Sets the most inner loop header `h` for a given node `b`
    *
    */
  private def tag_lhead(istate: State, b: Block, h: Block): State = {
    var cur1: Block = b
    var cur2: Block = h
    var st = istate

    if (cur1 == cur2) {
      return st // We don't consider self-loops to be loops for our purposes
    }

    var hasLoopHeader: Boolean = st.iloopHeaders.contains(cur1)
    while (hasLoopHeader) {
      val ih = st.iloopHeaders(cur1)
      if (ih == cur2) {
        return st
      }
      if (st.nodeDFSPpos(ih) < st.nodeDFSPpos(cur2)) {
        st = st.copy(iloopHeaders = st.iloopHeaders.updated(cur1, cur2))
        cur1 = cur2
        cur2 = ih
      } else {
        cur1 = ih
      }
      hasLoopHeader = st.iloopHeaders.contains(cur1)
    }
    st = st.copy(iloopHeaders = st.iloopHeaders.updated(cur1, cur2))
    st
  }
}


object LoopTransform {

  /* For every irreducible loop, transform it into a reducible one. The algorithm
   *  for doing this is "inspired" (read: copied) from LLVM, the source for which
   *  can be found here: https://llvm.org/doxygen/FixIrreducible_8cpp_source.html
   *
   * Returns: Set of loops which were previously irreducible, but are no longer
   */
  def llvm_transform(loops: Iterable[Loop]): Iterable[Loop] = {
    loops.map(l => {
      if (!l.reducible) {
        llvm_transform_loop(l)
      } else {
        l
      }
    }) 
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
  private def llvm_transform_loop(loop: Loop): Loop = {
    val entryEdges: Set[LoopEdge] = loop.entryEdges.union(loop.reentries).union(loop.backEdges).toSet

    val P_e: Set[LoopEdge] = entryEdges // N entry edges
    val P_b: Set[LoopEdge] = entryEdges.flatMap { e =>
      val predNodes = e.to.prevBlocks 
      predNodes.map(a => LoopEdge(a, e.to))
    } -- P_e // N back edges
    val body: Set[LoopEdge] = loop.edges.toSet -- P_b // Regular control flow in the loop

    // 3. Create block `N` and redirect every edge from set `P` to `H` via `N`
    val conns = P_e.map(e => e.to).union(P_b.map(e => e.to)).collect { case blk: Block => blk }
    val NGoTo = GoTo(conns)

    val N = Block(s"${loop.header.label}_loop_N", jump = NGoTo)
    IRWalk.procedure(loop.header).addBlocks(N)

    val newLoop = Loop(N)
    newLoop.edges ++= body

    P_e.foreach { originalEdge => {
        originalEdge.from.replaceJump(GoTo(List(N)))
        newLoop.addEdge(LoopEdge(originalEdge.from, N))
        newLoop.addEdge(LoopEdge(N, originalEdge.to))
      }
    }

    P_b.foreach { originalEdge => {
        originalEdge.from.replaceJump(GoTo(List(N)))
        val toEdge = LoopEdge(originalEdge.from, N)

        newLoop.addEdge(toEdge)
        newLoop.addEdge(LoopEdge(N, originalEdge.to))
        newLoop.backEdges += toEdge
      }
    }

    newLoop
  }
}
