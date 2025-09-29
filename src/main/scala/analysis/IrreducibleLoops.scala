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

/**
 * Loop identification and irreducible loop transformation.
 *
 * The [[IrreducibleLoops.transform_all_and_update]] method is the main entry
 * point for doing the identification and transformation in one step. This also
 * handles updating the loop information stored within blocks.
 *
 * Alternatively, the [[IrreducibleLoops.identify_loops]] method performs loop
 * identification, and results of this can be passed to
 * [[IrreducibleLoops.transform_loop]] to transform an irreducible loop to a
 * reducible one.
 *
 * The loop analysis produces a loop-nesting *forest* where nested sub-loops are
 * children of containing loops. A *sub-loop* of a loop is defined as a
 * strongly-connected component in the graph `V - {h}` where `V` is the set of
 * vertices (blocks) and `h` is the header of the parent loop. Examples of this
 * can be found in [the paper].
 *
 * Note that the loop-nesting tree is based on an arbitrary depth-first
 * traversal order and a CFG may have multiple valid loop-nesting trees.
 * In particular, certain nodes are chosen to be primary headers based
 * on this order. An irreducible loop, by definition, will have multiple
 * potential headers. We call the chosen header the _primary_ header
 * and any possible irreducible headers are called _secondary_ headers.
 *
 * For an overview of loop concepts and terminology, see LLVM's [Loop][]
 * and [Cycle][] pages. Note that we do not closely follow the terminology
 * of LLVM. For instance, we call both irreducible and reducible loops as
 * "loops", whereas LLVM only uses loop for *reducible* loops and uses cycle
 * for reducible or irreducible.
 *
 * [the paper]: http://dx.doi.org/10.1007/978-3-540-74061-2_11
 * [Loop]: https://llvm.org/docs/LoopTerminology.html
 * [Cycle]: https://llvm.org/docs/CycleTerminology.html
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

  def identify_all_loops(prog: Program): List[BlockLoopInfo] =
    prog.procedures.toList.flatMap(x => identify_loops(x).getOrElse(Nil))

  /**
   * Identifies loops then stores each block's loop information into the
   * [[ir.Block#loopInfo]] field of the [[ir.Block]].
   */
  def update_block_loop_info(prog: Program) =
    val loops = identify_all_loops(prog)
    loops.foreach { loop => loop.b.loopInfo = Some(loop) }
    loops

  /** A directed edge between two IR blocks. */
  case class LoopEdge(from: Block, to: Block)

  /**
  * Loop-related information for a particular block `b`. This includes whether
  * `b` participates in any loops, and whether `b` is a primary header
  * for a loop. This information is passed to [[transform_loop]] to transform
  * irreducible loops.
  *
  * This class is constructed from a [[BlockLoopState]] with
  * [[BlockLoopState.computeBlockLoopInfo]].
  *
  * @param b the block which this loop information concerns.
  * @param iloop_header if `b` is within a loop, this records the primary
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

    /** Whether the block is an irreducible loop's header. */
    val isIrreducible = headers.size > 1

    /** Whether the block is a reducible loop's header. */
    val isReducible = headers.size == 1

    /** Whether the block is a primary loop header. */
    val isLoopHeader = headers.nonEmpty

    /** Whether the block participates in any loop. */
    val isLoopParticipant = isLoopHeader || iloop_header.isDefined

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
  }

  /**
   * Temporary mutable state for block information within [[TraverseLoops]].
   * Converted to an immutable [[BlockLoopInfo]] once the traversal is complete
   * and the information is finalised.
   *
   * @param b block the state is relevant to
   * @param iloop_header if set, header of innermost loop containing this block
   * @param dfsp_pos if non-zero, index of this block in the current depth-first
   *                 shortest-path. if zero, block is not visited yet or visiting
   *                 the block has finished.
   * @param dfsp_pos_max the non-zero value of `dfsp_pos` when it was set. this
   *                     value is maintained even after visiting is finished.
   * @param is_traversed whether visiting this block has _started_.
   * @param headers headers of the loop headed by this block. if this block
   *                heads a loop, this always contains `b` as the primary
   *                header. for irreducible loops, it also contains secondary
   *                headers.
   */
  case class BlockLoopState(
    val b: Block,
    var iloop_header: Option[Block],
    var dfsp_pos: Int,
    var dfsp_pos_max: Int,
    var is_traversed: Boolean,
    var headers: Set[Block]
  ) {

    /**
    * Converts the mutable [[BlockLoopState]] into an immutable [[BlockLoopInfo]],
    * suitable for returning to the caller. Requires additional information which
    * has to be computed by considering multiple BlockLoopStates. This is handled
    * by [[BlockLoopState.computeBlockLoopInfo]].
    */
    def toBlockLoopInfo(nodes: Set[Block]) =
      BlockLoopInfo(b, iloop_header, dfsp_pos_max, headers, nodes)
  }

  /** Helper methods. Notably, includes [[BlockLoopState.computeBlockLoopInfo]]
   *  to convert a temporary BlockLoopState into a BlockLoopInfo suitable for
   *  transforming.
   */
  object BlockLoopState {

    /**
     * Computes [[BlockLoopInfo]] from the temporary [[BlockLoopState]]
     * information. This additionally computes the transitive node-sets for
     * each loop to populate the `nodes` field.
     */
    def computeBlockLoopInfo(blockStates: Map[Block, BlockLoopState]): List[BlockLoopInfo] = {
      // NOTE: loops are in *bottom-up topological order*.
      val allBlocks = blockStates.values.toList.sortBy(-_.dfsp_pos_max)

      val headerBlocks = allBlocks.collect {
        case loop if loop.headers.nonEmpty => loop
      }

      var forest: Map[Block, Set[Block]] = headerBlocks.iterator.map(b => b.b -> Set(b.b)).toMap

      // NOTE: iterates the forest in *bottom-up* topological order. this
      // ensures that node-sets of sub-cycles are fully populated before
      // processing their parent cycle. this avoids us having to compute
      // closures of node-sets.
      forest = allBlocks.foldLeft(forest) { case (forest, b) =>
        b.iloop_header match {
          case Some(h) => forest + (h -> (forest(h) + b.b))
          case None => forest
        }
      }

      // map of headers to internal blocks which have that as their innermost
      // loop header.
      val selfNodes = forest

      forest = headerBlocks.foldLeft(forest) { case (forest, b) =>
        b.iloop_header match {
          case Some(h) => forest + (h -> (forest(h) ++ forest(b.b)))
          case None => forest
        }
      }

      assert {
        headerBlocks.forall { b =>
          val nodes = forest(b.b)
          b.headers.forall(x => x.prevBlocks.exists(!nodes.contains(_)))
        }
      }

      val newLoops = allBlocks.map { x =>
        x.toBlockLoopInfo(forest.getOrElse(x.b, Set()))
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
      procedure.blocks.map(b => b -> BlockLoopState(b, None, 0, 0, false, Set())).toMap

    import scala.language.implicitConversions

    /**
     * Implicit conversion to allow accessing members of [[BlockLoopState]] via
     * `.` on [[ir.Block]] values. This lets us write code which looks closer to
     * the algorithm in the paper.
     */
    given Conversion[Block, BlockLoopState] = loopBlocks.apply

    /** Main entry point for the loop identification algorithm. Calls
     *  [[trav_loops_tailrec]] with the appropriate arguments. Returns a
     *  list of [[BlockLoopInfo]] if successful, or `None` if the procedure has
     *  no entry block. The returned list will be in topological order - outer
     *  cycles appear _before_ their subcycles.
     *
     *  This method should be called at most once on each [[TraverseLoops]]
     *  object.
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

        // if there is nothing remaining in the stack, this is the outer-most
        // call. simply return.
        case (Right(nh), Nil) => return nh
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
            b.headers += b.b
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

              h.headers += b.b

              var continue = true
              while (continue && h.iloop_header.isDefined) {
                h = h.iloop_header.get
                if (h.dfsp_pos > 0) {
                  tag_lhead(b0, Some(h))
                  continue = false
                } else {
                  h.headers += b.b
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
      trav_loops_tailrec(Right(result), continuations)
    }

    /** Helper function described in the paper and called by [[trav_loops_tailrec]].
     *  Sets `h` as the loop header for the block `b` and all containing loops.
     */
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

  /**
   * This method is the main entry point for doing the identification and
   * transformation in one step. This also handles updating the loop
   * information stored within blocks.
   */
  def transform_all_and_update(prog: Program) = {
    prog.procedures.foreach { p =>
      identify_loops(p).foreach(transform_many_loops)
    }
    val newLoops = update_block_loop_info(prog)
    assert(newLoops.forall(!_.isIrreducible))
  }

  def transform_many_loops(loops: Iterable[BlockLoopInfo]) =
    loops.toList.sortBy(_.dfsp_pos).flatMap {
      IrreducibleLoops.transform_loop
    }

  def transform_loop(loop: BlockLoopInfo): Option[IrreducibleTransformInfo] = {
    if (!loop.isIrreducible) return None

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

    // internal edges to the primary header are also redirected through the new header.
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

    // for predecessors of the old primary header, replace their gotos with the new header.
    (externalEntries.iterator ++ backEdgesToFirstHeader).foreach { case edge @ LoopEdge(from, to) =>
      from.jump match {
        case goto: GoTo => goto.replaceTarget(to, newHeader)
        case _ => throw new Exception(s"edge $edge into loop was terminated by non-goto?!")
      }
    }

    Some(IrreducibleTransformInfo(newHeader, fromVariable, entryIndices, precedingIndices))
  }

}
