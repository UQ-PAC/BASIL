package analysis

import ir.{Assume, BinaryExpr, Block, BoolOR, EQ, GoTo, IRWalk, IntLiteral, IntType, LocalAssign, LocalVar, Program}

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

  def this(header: Block, reentries: Iterable[LoopEdge], backEdges: Iterable[LoopEdge], entryEdges: Iterable[LoopEdge], nodes: Iterable[Block], edges: Iterable[LoopEdge], reducible: Boolean) = {
    this(header)
    this.reentries ++= reentries
    this.backEdges ++= backEdges
    this.entryEdges ++= entryEdges
    this.nodes ++= nodes
    this.edges ++= edges
    this.reducible = reducible
  }

  def addEdge(edge: LoopEdge): Unit = {
    nodes += edge.from
    nodes += edge.to
    edges += edge
  }

  def name: String = label(header)

  def canonicalise(): Loop = {
    val possibleHeaders = header :: reentries.map(_.to).toList
    val leastHeader = possibleHeaders.minBy(b => (b.address, b.meta.originalLabel, b.label))

    if (header eq leastHeader) return this

    val allEntries = entryEdges.toSet ++ reentries

    val (entryEdgesNew, reentriesNew) = allEntries.partition(_.to eq leastHeader)

    new Loop(
      header = leastHeader,
      entryEdges = entryEdgesNew,
      reentries = reentriesNew,
      backEdges = edges.filter(_.to eq leastHeader),
      nodes = nodes,
      edges = edges,
      reducible = reducible
    )
  }

  override def toString: String = {
    s"Header: ${label(header)}, Body: $edges, Reentries: $reentries"
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
    private[LoopDetector] val visitedNodes: Set[Block] = Set(),
    private[LoopDetector] val nodeDFSPpos: Map[Block, Int] = Map(),
    private[LoopDetector] val iloopHeaders: Map[Block, Block] = Map(),
    private[LoopDetector] val edgeStack: List[LoopEdge] = List()
  ) {
    def irreducibleLoops: Set[Loop] = loops.values.filter(l => !l.reducible).toSet

    def identifiedLoops: Iterable[Loop] = loops.values

    def reducibleTransformIR(): State = {
      this.copy(loops = LoopTransform.llvm_transform(loops.values).map(l => l.header -> l).toMap)
    }

    def updateIrWithLoops() = {
      for ((hd, l) <- loops) {
        hd.inLoop = Set(l)
        for (participant <- l.nodes) {
          participant.inLoop = participant.inLoop + l
        }
      }
    }

    def canonicalise() = {
      val newLoops = loops.values.map(_.canonicalise())
      val loopMap = newLoops.map(x => x.header -> x).toMap
      State(
        loops = loopMap,
        headers = loopMap.keys.toSet
      )
    }
  }

  def identify_loops(entryBlock: Block): State = {
    var x = traverse_loops_dfs(State(), entryBlock, 1)
    x = x.copy(
      visitedNodes = Set(),
      nodeDFSPpos = Map(),
      iloopHeaders = Map(),
      edgeStack = List()
    )
    println(x)
    println(x.canonicalise())
    println("X")
    x.canonicalise()
  }

  /*
   * Returns the set of loops for each procedure in the program.
   */
  def identify_loops(cfg: Program): State = {
    var x = cfg.procedures.toSet
      .flatMap(_.entryBlock)
      .foldLeft(State())((st, eb) => traverse_loops_dfs(st, eb, 1))
    x = x.copy(
      visitedNodes = Set(),
      nodeDFSPpos = Map(),
      iloopHeaders = Map(),
      edgeStack = List()
    )
    println(x)
    println(x.canonicalise())
    println()
    x.canonicalise()
  }

  private def processVisitedNodeOutgoingEdge(istate: State, edge: LoopEdge): State = {
    var st = istate
    val from = edge.from
    if (st.nodeDFSPpos(edge.to) > 0) {
      // Case (b)
      // b is in DFSP(edge.from)
      st = st.copy(headers = st.headers + edge.to)

      val newLoop = st.loops.getOrElse(edge.to, Loop(edge.to))
      st.edgeStack.reverse.slice(st.nodeDFSPpos(edge.to) - 1, st.nodeDFSPpos(edge.from)).foreach { pEdge =>
        newLoop.addEdge(pEdge)
      }
      newLoop.backEdges += edge

      // Add loop entry edge
      edge.to.prevBlocks.foreach { predNode =>
        val predEdge = LoopEdge(predNode, edge.to)
        if (st.nodeDFSPpos.contains(predNode)) {
          if (st.nodeDFSPpos(predNode) > 0 && predEdge != edge) {
            newLoop.entryEdges += predEdge
          }
        }
      }

      if (!st.loops.contains(edge.to)) {
        st = st.copy(loops = st.loops.updated(edge.to, newLoop))
      }

      st = tag_lhead(st, edge.from, edge.to)
    } else if (!st.iloopHeaders.contains(edge.to)) {
      // Case (c) - do nothing
      // edge.to is not part of this path, and it's not a header, so we can ignore it.
      // (in effect we add it to our path later, but we do that when we discover a new header
      //  by looking through the instruction call stack)
    } else {
      var h: Block = st.iloopHeaders(edge.to)
      if (st.nodeDFSPpos(h) > 0) {
        // Case (d)
        // h is in DFSP(edge.from)
        val loop = st.loops(h)

        // Add current path to the existing loop (a new path in the loop is discovered)
        // This can happen for example in the case that there is a branch in a loop, or a `continue` stmt, etc
        st.edgeStack.reverse.slice(st.nodeDFSPpos(h) - 1, st.nodeDFSPpos(edge.from)).foreach(loop.addEdge)
        edge.to.nextBlocks.filter(n => n == h).foreach { n =>
          val outEdge = LoopEdge(edge.to, n)
          loop.addEdge(outEdge)
        }

        st = tag_lhead(st, edge.from, h)
      } else {
        // Case (e)
        // reentry (irreducible!)
        val loop = st.loops(h)
        loop.reducible = false
        loop.reentries += edge

        // Make outer loops irreducible if the originating node of the re-entry edge is not within those loops
        edge.to.nextBlocks.foreach { nextNode =>
          val nextEdge = LoopEdge(edge.to, nextNode)
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
            st = tag_lhead(st, edge.from, h)
            break = true
          }
        }
      }
    }

    // Here the most recent edge will be originating from `edge.from`
    st = st.copy(edgeStack = st.edgeStack match {
      case h :: tl => tl
      case Nil => Nil
    })
    st
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
  private def traverse_loops_dfs(_istate: State, _b0: Block, _DFSPpos: Int): State = {

    /** The recursion is flattened using a state machine operating over the stack of operations
      * to perform
      *
      *      +--> BeginProcessNode <--+     Start DFS on node: push ContinueDFS with node's successors
      *      |           |            |
      *      |           |       succ not visited
      *      |           |    (push self & successor)
      *      |           v            |
      *      |       ContinueDFS -----+     if not previously visited, traverse to successors (a),
      *      |           |            |     otherwise process this edge (b-e) then continue procesing edges.
      *      |      succ visited      |
      *  siblings        |       no successors           (siblings = processingEdges)
      *      |           V            |
      *      +---- ProcesVisitedNode  |      we have returned after processing successor edges
      *                  |            |
      *              no siblings      |
      *                  |            |
      *                  v            |
      *          FinishProcessNode <--+     all outgoing edges have been processed, finish this node
      */
    enum Action {
      case BeginProcessNode
      case ContinueDFS
      case ProcessVisitedNode
      case FinishProcessNode
    }

    case class LocalState(istate: State, b0: Block, pos: Int, action: Action, processingEdges: List[LoopEdge])
    val stack = mutable.Stack[LocalState]()
    stack.push(LocalState(_istate, _b0, _DFSPpos, Action.BeginProcessNode, List()))

    var retval: (State, Option[Block]) = (_istate, None)

    while (stack.nonEmpty) {
      var sf = stack.pop()
      var st = sf.istate

      sf.action match {
        case Action.BeginProcessNode =>
          st = st.copy(visitedNodes = st.visitedNodes + sf.b0)
          st = st.copy(nodeDFSPpos = st.nodeDFSPpos.updated(sf.b0, sf.pos))
          val edgesToProcess = sf.b0.nextBlocks.map(toNode => LoopEdge(sf.b0, toNode)).toList
          sf = sf.copy(istate = st, processingEdges = edgesToProcess, action = Action.ContinueDFS)
          stack.push(sf)

        case Action.ContinueDFS =>
          sf.processingEdges match {
            case edge :: tl =>
              sf = sf.copy(processingEdges = tl)

              if (!st.visitedNodes.contains(edge.to)) {
                // (a) not visited before: BeginProcessNode on this successor
                st = st.copy(edgeStack = edge :: st.edgeStack)
                stack.push(sf.copy(action = Action.ProcessVisitedNode)) // continue after processing this successor
                stack.push(
                  sf.copy(istate = st, b0 = edge.to, pos = sf.pos + 1, action = Action.BeginProcessNode)
                ) // process this successor
              } else {
                // (b-e) visited before: finish processing this edge
                st = processVisitedNodeOutgoingEdge(st, edge)
                // continue iterating `processingEdges`
                stack.push(sf.copy(istate = st, action = Action.ContinueDFS))
              }
            case Nil =>
              // loop over
              sf = sf.copy(action = Action.FinishProcessNode)
              stack.push(sf)
          }

        case Action.ProcessVisitedNode =>
          val (nst, newhead) = retval
          st = nst
          st = newhead.foldLeft(st)((st, b) => tag_lhead(st, sf.b0, b))
          sf = sf.copy(action = Action.ContinueDFS, istate = st) // continue iteration
          stack.push(sf)

        case Action.FinishProcessNode =>
          st = st.copy(nodeDFSPpos = st.nodeDFSPpos.updated(sf.b0, 0))
          retval = (st, st.iloopHeaders.get(sf.b0))

      }
    }

    retval(0)
  }

  /** Sets the most inner loop header `h` for a given node `b`
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
    loops.map { l =>
      if (!l.reducible) {
        llvm_transform_loop(l)
      } else {
        l
      }
    }
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

    // Add bookeeping to preserve control flow precision across the introduced N block
    // WARNING: We are not careful to avoid repeatedly adding these statements to the IR, so
    // re-running this transform following a transform producing irreducible control flow over the
    // same entry blocks will produce invalid/unreachable code
    val entrys = P_e.map(_.from)
    val entryids = entrys.zip(0 until entrys.size).toMap

    for ((block, id) <- entryids) {
      block.statements.prepend(LocalAssign(LocalVar("FromEntryIdx", IntType), IntLiteral(BigInt(id))))
    }

    P_e.groupBy(_.to).map { (destBlock, origins) =>
      val idexs = origins.map { b =>
        BinaryExpr(EQ, LocalVar("FromEntryIdx", IntType), IntLiteral(BigInt(entryids(b.from))))
      }
      idexs.toList match {
        case Nil => ()
        case h :: tl =>
          val cond = tl.foldLeft(h)((l, r) => BinaryExpr(BoolOR, l, r))
          destBlock.statements.prepend(Assume(cond))
      }
    }

    // 3. Create block `N` and redirect every edge from set `P` to `H` via `N`
    val conns = P_e.map(e => e.to).union(P_b.map(e => e.to)).collect { case blk: Block => blk }
    val NGoTo = GoTo(conns)

    val N = Block(s"${loop.header.label}_loop_N", jump = NGoTo)
    IRWalk.procedure(loop.header).addBlock(N)

    val newLoop = Loop(N)
    newLoop.edges ++= body

    P_e.foreach { originalEdge =>
      originalEdge.from.replaceJump(GoTo(List(N)))
      newLoop.addEdge(LoopEdge(originalEdge.from, N))
      newLoop.addEdge(LoopEdge(N, originalEdge.to))
    }

    P_b.foreach { originalEdge =>
      originalEdge.from.replaceJump(GoTo(List(N)))
      val toEdge = LoopEdge(originalEdge.from, N)

      newLoop.addEdge(toEdge)
      newLoop.addEdge(LoopEdge(N, originalEdge.to))
      newLoop.backEdges += toEdge
    }

    newLoop
  }
}
