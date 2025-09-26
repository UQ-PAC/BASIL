package analysis

import ir.{
  AssocExpr,
  Assume,
  BinaryExpr,
  Block,
  BoolOR,
  EQ,
  GoTo,
  IntLiteral,
  IntType,
  LocalAssign,
  LocalVar,
  Procedure,
  Program,
  Unreachable
}
import util.assertion.*

import scala.annotation.tailrec
import scala.collection.mutable

private def label(p: Block) = "block." + p.label

/* A connection between to IL nodes, purely for the representation of loops in
 *
 */
case class LoopEdge(from: Block, to: Block) {
  override def toString: String = s"(${label(from)}, ${label(to)})"
}

/**
 * A loop is a strongly-connected component in the block graph.
 */
class Loop(val header: Block) {

  /** Edges to loop from outside that are not to the header.
   *  This set is non-empty if and only if the loop is *irreducible*.
   */
  val reentries: mutable.Set[LoopEdge] = mutable.Set()

  /** Edges from inside loop to the header. */
  val backEdges: mutable.Set[LoopEdge] = mutable.Set()

  /** Edges into the header node from outside the loop. */
  val entryEdges: mutable.Set[LoopEdge] = mutable.Set()

  /** Nodes making up the loop. These form a maximal strongly-connected component. */
  val nodes: mutable.Set[Block] = mutable.Set() // G_l
  /** Edges internal to the loop, i.e., between two loop [[nodes]]. */
  val edges: mutable.Set[LoopEdge] = mutable.Set() // G_e
  var reducible: Boolean = true // Assume reducible by default

  def addEdge(edge: LoopEdge): Unit = {
    nodes += edge.from
    nodes += edge.to
    edges += edge
  }

  def name: String = label(header)

  override def toString: String = List(
    s"\nHeader: ${label(header)}",
    s"Reducible: $reducible",
    s"Body: $nodes",
    s"Entry edges: $entryEdges",
    s"Back edges: $backEdges",
    s"Reentries: $reentries"
  ).mkString("\n")

  def checkLoopValidity(): Boolean = {
    val edges = reentries.iterator ++ backEdges ++ entryEdges ++ this.edges
    val missingEdges = edges.filter { case LoopEdge(from, to) =>
      !from.nextBlocks.iterator.contains(to)
    }.toList

    missingEdges.isEmpty
  }
}

/**
 * Loop identification and irreducible loop transformation.
 *
 * The [[IrreducibleLoops.identify_loops]] method performs loop identification,
 * and results of this can be passed to [[IrreducibleLoops.transform_loop]]
 * to transform an irreducible loop to a reducible one.
 */
object IrreducibleLoops {

  /** Main entry point for the loop identification algorithm. Instantiates
   *  [[TraverseLoops]] with the appropriate arguments. Returns a
   *  list of [[BlockLoopInfo]] if successful, or `None` if the procedure has
   *  no entry block. The returned list will be in a topological order - outer
   *  cycles appear _before_ their subcycles.
   */
  def identify_loops(procedure: Procedure): Option[List[BlockLoopInfo]] =
    TraverseLoops(procedure).traverse_loops()

  def identify_loops(prog: Program): List[BlockLoopInfo] =
    prog.procedures.toList.flatMap(x => identify_loops(x).getOrElse(Nil))

  /**
  * Loop-related information for a particular block `b`. This includes whether
  * `b` participates in any loops, and whether `b` is a distinguished header
  * for a loop. This information is passed to [[transform_loop]] to transform
  * irreducible loops.
  *
  * The loop analysis produces a loop-nesting *tree* where nested sub-loops are
  * children of containing loops. A *sub-loop* of a loop is defined as a
  * strongly-connected component in the graph `V - {h}` where `V` is the set of
  * vertices (blocks) and `h` is the header of the parent loop.
  *
  * Note that the loop-nesting tree is based on an arbitrary depth-first
  * traversal order and a CFG may have multiple valid loop-nesting trees.
  * In particular, certain nodes are chosen to be distinguished headers based
  * on this order. An irreducible loop, by definition, will have multiple
  * potential headers.
  *
  * This class is constructed from a [[BlockLoopState]] with
  * [[BlockLoopState#toBlockLoopInfo]].
  *
  * @param b the block which this loop information concerns.
  * @param iloop_header if `b` is within a loop, this records the distinguished
  *                     header of the innermost loop containing `b`. otherwise,
  *                     it is None.
  * @param dfsp_pos visit order index of `b` within the depth-first traversal.
  *                 within the loop-nesting tree, parents have a _lesser_ value
  *                 of `dfsp_pos` than all their children.
  * @param headers if `b` is a loop header, this stores the set of blocks which
  *                can be used to enter the loop. a header is defined as
  *                dominating all non-header nodes of the loop.
  * @param nodes if `b` is a loop header, this stores the set of blocks which
  *              are internal to the loop. this set forms a strongly-connected
  *              component. note that a block may be internal to multiple loops.
  */
  case class BlockLoopInfo(
    val b: Block,
    val iloop_header: Option[Block],
    val dfsp_pos: Int,
    val headers: Set[Block],
    val nodes: Set[Block]
  ) {
    def isIrreducible() = headers.size > 1
    def isCycle() = headers.nonEmpty

    /** Accesses the Basil IR state to compute the set of entry edges
     *  originating from outside the loop and going towards *any* header of the
     *  loop.
     */
    def computeEntries() =
      headers.flatMap(h => (h.prevBlocks.toSet -- nodes).map(LoopEdge(_, h)))

    /** Accesses the Basil IR state to compute the set of back-edges. That is,
     *  the set of edges originating from _inside_ the loop and going towards
     *  *any* header of the loop.
     */
    def computeBackEdges() =
      headers.flatMap(h => (h.prevBlocks.toSet & nodes).map(LoopEdge(_, h)))

    /** Converts the [[BlockLoopInfo]] into a [[Loop]] by inspecting the
     *  *current* block edges in the Basil IR CFG. As such, the result of this
     *  method depends on the current IR state and may become invalidated if
     *  changes are made to the IR after it is called.
     *
     *  If the current [[BlockLoopInfo]] is not the distinguished header of a
     *  loop, returns None.
     */
    def toLoop(): Option[Loop] = {
      if (!isCycle()) return None

      val loop = Loop(b)

      loop.reentries ++= (headers - b).flatMap(h => (h.prevBlocks.toSet -- nodes).map(LoopEdge(_, h)))
      loop.backEdges ++= (Set(b) & headers).flatMap(h => (h.prevBlocks.toSet & nodes).map(LoopEdge(_, h)))
      loop.entryEdges ++= (Set(b) & headers).flatMap(h => (h.prevBlocks.toSet -- nodes).map(LoopEdge(_, h)))
      loop.nodes ++= nodes
      loop.edges ++= nodes.flatMap(n => (n.nextBlocks.toSet & nodes).map(LoopEdge(n, _)))
      loop.reducible = loop.reentries.isEmpty
      Some(loop)
    }
  }

  /**
   * Temporary mutable state for block information within [[TraverseLoops]].
   * Converted to an immutable [[BlockLoopInfo]] once the traversal is complete
   * and the information is finalised.
   *
   * @param possibleReentries
   *   re-entry into this loop to a secondary header. note that this is only
   *   populated for the _innermost_ loop which is re-entered. later, in
   *   [[BlockLoopState.computeBlockLoopInfo]], this is attached to other
   *   containing loops as well.
   */
  case class BlockLoopState(
    val b: Block,
    var iloop_header: Option[Block],
    var is_header: Boolean,
    var dfsp_pos: Int,
    var dfsp_pos_max: Int,
    var is_traversed: Boolean,
    var possibleReentries: Set[LoopEdge]
  ) {

    /**
    * Converts the mutable [[BlockLoopState]] into an immutable [[BlockLoopInfo]],
    * suitable for returning to the caller.
    */
    def toBlockLoopInfo(nodes: Set[Block], headers: Set[Block]) =
      BlockLoopInfo(b, iloop_header, dfsp_pos_max, headers, nodes)
  }

  /** Helper methods. Notably, includes [[BlockLoopState.computeBlockLoopInfo]]
   *  to convert a temporary BlockLoopState into a BlockLoopInfo suitable for
   *  transforming.
   */
  object BlockLoopState {

    def computeBlockLoopInfo(loopStates: Map[Block, BlockLoopState]): List[BlockLoopInfo] = {
      // NOTE: loops are in *bottom-up topological order*.
      val loops = loopStates.values.toList.sortBy(-_.dfsp_pos_max)

      var forest = Map[Block, Set[Block]]()
      forest = loops.foldLeft(forest) { case (forest, b) =>
        forest ++ Option.when(b.is_header)(b.b -> Set(b.b))
      }

      // NOTE: iterates the forest in *bottom-up* topological order. this
      // ensures that node-sets of sub-cycles are fully populated before
      // processing their parent cycle. this avoids us having to compute
      // closures of node-sets.
      forest = loops.foldLeft(forest) { case (forest, b) =>
        forest ++ b.iloop_header.map(h => h -> (forest(h) + b.b))
      }

      // map of headers to internal blocks which have that as their innermost
      // loop header.
      val selfNodes = forest

      forest = loops.foldLeft(forest) { case (forest, b) =>
        forest ++ b.iloop_header.map(h => h -> (forest(h) ++ forest.getOrElse(b.b, Nil)))
      }

      // we need to hoist possible re-entries up to and including
      // the outermost loop which is a parent of edge.from which does
      // not contain edge.to
      val hoistedEntries = loops.foldLeft(Map[Block, Set[LoopEdge]]()) { (acc, loop) =>
        var hoistedEntries = acc

        val loopHoistedEntries = hoistedEntries.getOrElse(loop.b, Set())
        hoistedEntries -= loop.b

        // slice the re-entry edges into those which will be hoisted and those which
        // will not. hoisted edges will also be applied to the containing loop.
        val (thisLoopEntries, toHoist) = (loop.possibleReentries ++ loopHoistedEntries).partitionMap {
          case edge @ LoopEdge(_, to) if to == loop.b => Left(edge)
          case edge @ LoopEdge(from, to) =>
            loop.iloop_header.map(h => (h, forest(h))) match {
              case None => Left(edge)
              case Some((_, outerNodes)) if outerNodes.contains(from) => Left(edge)
              case Some((h, _)) => Right(edge)
            }
        }

        // add normal entries to the header which are not stored
        val simpleEntries = if (loop.is_header) {
          val nodes = forest(loop.b)
          (loop.b.prevBlocks.toSet -- nodes).map(LoopEdge(_, loop.b))
        } else {
          Set()
        }

        hoistedEntries += loop.b -> (thisLoopEntries ++ simpleEntries ++ toHoist)
        loop.iloop_header match {
          case Some(h) => hoistedEntries += h -> (toHoist ++ hoistedEntries.getOrElse(h, Nil))
          case None => assert(toHoist.isEmpty, "attempting to hoist entries but there is no parent loop!")
        }
        hoistedEntries
      }
      // println("HOIST" + hoistedEntries.filter(_._2.nonEmpty))

      val newLoops = loops.map { x =>
        x.toBlockLoopInfo(forest.getOrElse(x.b, Set()), hoistedEntries.getOrElse(x.b, Set()).map(_.to))
      }

      // NOTE: reverse order before returning, so outer loops appear first.
      newLoops.reverse
    }

  }

  /** Performs the DFS-based loop analysis as described in [1]. Each instance of
   *  this should be used at most once. The [[identify_loops]]
   *  function will construct this class for you and call the appropriate methods.
   *
   *  [1] T. Wei, J. Mao, W. Zou, and Y. Chen, “A New Algorithm for Identifying
   *  Loops in Decompilation,” Lecture Notes in Computer Science. Springer
   *  Berlin Heidelberg, pp. 170–183, 2007. doi: 10.1007/978-3-540-74061-2_11.
   *  Available: http://dx.doi.org/10.1007/978-3-540-74061-2_11
   */
  class TraverseLoops(val procedure: Procedure) {

    var used = false
    val loopBlocks: Map[Block, BlockLoopState] =
      procedure.blocks.map(b => b -> BlockLoopState(b, None, false, 0, 0, false, Set())).toMap

    import scala.language.implicitConversions

    given Conversion[Block, BlockLoopState] with
      def apply(b: Block) = loopBlocks(b)

    /** Main entry point for the loop identification algorithm. Calls
     *  [[trav_loops_tailrec]] with the appropriate arguments. Returns a
     *  [[scala.collection.immutable.ListMap]] of [[BlockLoopInfo]] if
     *  successful, or `None` if the procedure has no entry block. The returned
     *  `ListMap` will be in topological order - outer cycles occur _before_
     *  their subcycles.
     */
    def traverse_loops() = {
      require(!used, "cannot call traverse_loops twice")
      used = true
      procedure.entryBlock.map { entry =>
        val _ = this.trav_loops_tailrec(Left((loopBlocks(entry), 1)), Nil)
        BlockLoopState.computeBlockLoopInfo(loopBlocks)
      }
    }

    /**
     * Tail-recursive form of the DFS-based traversal described in the paper.
     *
     * The original algorithm has one recursive call, so its tail-recursive
     * form has two "entry points" - one from the beginning of the function,
     * and one when a recursive subcall has returned and wants to continue.
     * This is implemented by using an [[scala.util.Either]] parameter. The
     * `Left` case denotes a normal call to the function with arguments `(b0,
     * dfsp_pos)`, and the `Right` case denotes a return from a recursive
     * subcall with arguments `nh`, the return value of the recursive subcall.
     *
     * This is combined with a stack of nested calls. A recursive "call"
     * happens by invoking the function with `Left` and pushing the current
     * `(b0, dfsp_pos, it)` onto the stack. In particular, storing the `it`
     * iterator lets us resume the iteration at a later point. Upon completing
     * execution of one call to the function, if the stack is non-empty, the
     * function will "return" to the parent call by invoking the function with
     * a `Right` argument.
     */
    @tailrec
    final def trav_loops_tailrec(
      input: Either[(BlockLoopState, Int), Option[BlockLoopState]],
      inputContinuations: List[(BlockLoopState, Int, Iterator[BlockLoopState])]
    ): Option[BlockLoopState] = {

      val (b0, dfsp_pos, it, continuations) = (input, inputContinuations) match {

        // Left denotes a normal function entry. The code here is in the entry
        // of the paper's algorithm, before the loop begins.
        case (Left((b0, dfsp_pos)), conts) => {
          b0.dfsp_pos = dfsp_pos
          b0.dfsp_pos_max = dfsp_pos
          b0.is_traversed = true
          val it = b0.b.nextBlocks.map(loopBlocks(_)).iterator
          (b0, dfsp_pos, it, conts)
        }

        // Right denotes a return from a recursive subcall. This simply calls
        // tag_lhead, which appears in the algorithm after the recursive
        // subcall, then continues the iteration using the stored `it`.
        case (Right(nh), (b0, dfsp_pos, it) :: rest) =>
          tag_lhead(b0, nh)
          (b0, dfsp_pos, it, rest)

        case (Right(_), Nil) =>
          throw new Exception("trav_loops_tailrec: stack underflow")
      }

      while (it.hasNext) {
        val b = it.next()
        if (!b.is_traversed) {
          return trav_loops_tailrec(Left((b, dfsp_pos + 1)), (b0, dfsp_pos, it) :: continuations)
          /* before tailrec transformation:
           *
           * val nh = trav_loops_dfs(b, dfsp_pos + 1)
           * tag_lhead(b0, nh)
           */
        } else {
          if (b.dfsp_pos > 0) {
            // println("mark as loop header: " + b + " from " + b0)
            b.is_header = true
            tag_lhead(b0, Some(b))
          } else if (b.iloop_header.isEmpty) {
            // intentionally empty
          } else {
            var h = b.iloop_header.get
            if (h.dfsp_pos > 0) {
              tag_lhead(b0, Some(h))
            } else {
              // println(s"IRRED: mark $b0 as re-entry into $b. irreducible.")

              val h0 = h

              h.possibleReentries = h.possibleReentries + LoopEdge(b0.b, b.b)

              var continue = true
              while (continue && h.iloop_header.isDefined) {
                h = h.iloop_header.get
                if (h.dfsp_pos > 0) {
                  tag_lhead(b0, Some(h))
                  continue = false
                }
              }
              // println("continue: " + continue)

              // println("h0: " + h0)
              // println("h: " + h)
              // println("b0: " + b0)
              // println("b: " + b)
            }
          }
        }
      }
      b0.dfsp_pos = 0
      val result = b0.iloop_header.map(loopBlocks(_))
      continuations match {
        case Nil => result
        case _ :: _ => trav_loops_tailrec(Right(result), continuations)
      }
    }

    /** Helper function described in the paper and called by [[trav_loops_tailrec]]. */
    def tag_lhead(b: BlockLoopState, h: Option[BlockLoopState]): Unit = h match {
      case Some(h) if b.b ne h.b =>
        var cur1 = b
        var cur2 = h
        while (cur1.iloop_header.isDefined) {
          val ih = cur1.iloop_header.get
          if (ih eq cur2.b) return
          if (ih.dfsp_pos < cur2.dfsp_pos) {
            cur1.iloop_header = Some(cur2.b)
            cur1 = cur2
            cur2 = ih
          } else {
            cur1 = ih
          }
        }
        cur1.iloop_header = Some(cur2.b)
      case _ => ()
    }

  }

  /**
   * Information about transforms made to convert an irreducible loop into
   * a reducible one.
   *
   * @param newHeader the new (unique) header for the loop. after the transform, this
   *                  is the header of the *reducible* version of this loop.
   * @param fromVariable the fresh variable which is used to determine which old header to jump to
   *                     when reaching the new header. this variable stores an integer index.
   * @param entryIndices map of entry blocks (i.e., blocks preceding headers) to their
   *                     integer index.
   * @param precedingIndices map of (old) header blocks to the entry indices which precede
   *                         that header.
   */
  case class IrreducibleTransformInfo(
    val newHeader: Block,
    val fromVariable: LocalVar,
    val entryIndices: Map[Block, Int],
    val precedingIndices: Map[Block, List[Int]]
  )

  def transform_all_loops(prog: Program) =
    transform_many_loops(identify_loops(prog))

  def transform_many_loops(loops: Iterable[BlockLoopInfo]) = {
    loops.toList.sortBy(_.dfsp_pos).flatMap { loop =>
      IrreducibleLoops.transform_loop(loop)
    }
  }

  def transform_loop(loop: BlockLoopInfo): Option[IrreducibleTransformInfo] = {
    if (!loop.isIrreducible()) return None

    // From LLVM: https://llvm.org/doxygen/FixIrreducible_8cpp_source.html
    //
    // > To convert an irreducible cycle C to a natural loop L:
    // >
    // > 1. Add a new node N to C.
    // > 2. Redirect all external incoming edges through N.
    // > 3. Redirect all edges incident on header H through N.
    // >
    // > This is sufficient to ensure that:
    // >
    // > a. Every closed path in C also exists in L, with the modification that any
    // >    path passing through H now passes through N before reaching H.
    // > b. Every external path incident on any entry of C is now incident on N and
    // >    then redirected to the entry.
    //
    // In (3.), we take "incident on H" to mean internal edges pointing to H.

    val header = loop.b
    val procedure = header.parent

    // in the transform, all external entries are redirected to the new header.
    val externalEntries = loop.computeEntries()

    val backEdges = loop.computeBackEdges()

    // internal edges to the first header are also redirected through the new header.
    // note this excludes internal edges to alternative headers.
    val backEdgesToFirstHeader: Set[LoopEdge] = backEdges.filter(_.to == header)

    // compute entries into any of the old headers. keyed by entry blocks.
    val entryIndices: Map[Block, Int] = (externalEntries ++ backEdges).map(_.from).zipWithIndex.toMap
    // included entry blocks should be a superset of (externalEntries ++ backEdgesToFirstHeader).
    // in particular, it additionally includes internal edges to alternative headers.
    assert((externalEntries.toSet ++ backEdgesToFirstHeader).map(_.from).subsetOf(entryIndices.keys.toSet))

    // indices of blocks which may go to a particular header. keyed by header block.
    val precedingIndices: Map[Block, List[Int]] =
      (externalEntries ++ backEdges).toList.groupMap(_.to)(_.from).map { (h, prevs) =>
        h -> prevs.map(entryIndices(_)).sorted
      }

    // new header is succeeded by all the old headers.
    // NOTE: created after oldHeaders are iterated to make sure newHeader doesn't appear in those maps.
    val newHeader = Block(s"${header.label}_loop_N", jump = Unreachable())
    val oldHeaders = externalEntries.map(_.to)
    newHeader.replaceJump(GoTo(oldHeaders))
    procedure.addBlock(newHeader)

    // the "from" variable is assigned based on blocks which enter the loop.
    // entering blocks might enter a *subset* of the headers, and this needs to
    // be maintained when redirecting through the new header.
    //
    // as such, each entering block gets its own value for this variable
    // and this is disjoined into an assume statement within each header.
    //
    // this includes both external and internal entries.
    val fromVariable = LocalVar(s"${header.label}_loop_from", IntType)

    // transform each old header by adding an assume of the blocks which might enter it.
    precedingIndices.foreach { (h, indices) =>
      val eqs = indices.map(i => BinaryExpr(EQ, fromVariable, IntLiteral(BigInt(i))))
      h.statements.prepend(Assume(AssocExpr(BoolOR, eqs)))
    }

    // for every predecessor of all old headers, assign its from variable. this makes
    // sure that the assume works.
    entryIndices.foreach { (entry, i) =>
      entry.statements.append(LocalAssign(fromVariable, IntLiteral(BigInt(i))))
    }

    // for predecessors of the distinguished old header, replace their gotos with the new header.
    (externalEntries.iterator ++ backEdgesToFirstHeader).foreach { case edge @ LoopEdge(from, to) =>
      from.jump match {
        case goto: GoTo => goto.replaceTarget(to, newHeader)
        case _ => throw new Exception(s"edge $edge into loop was terminated by non-goto?!")
      }
    }

    Some(IrreducibleTransformInfo(newHeader, fromVariable, entryIndices, precedingIndices))
  }

}
