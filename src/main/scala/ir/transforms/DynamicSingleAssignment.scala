package ir.transforms

import ir.*
import ir.cilvisitor.*
import translating.*
import translating.PrettyPrinter.*
import util.Logger
import util.assertion.*

import scala.collection.mutable

val phiAssignLabel = Some("phi")

/** This transforms the program by adding no-op copies and renaming local variable indices to establish the property
  * that
  *
  * \forall variables v, forall uses of v : u, No subset of definitions of v defines the use u.
  */

class OnePassDSA(
  /** Check our (faster) live var result against the TIP sovler solution
    */
) {

  val liveVarsDom = transforms.IntraLiveVarsDomain()
  val liveVarsSolver = transforms.worklistSolver(liveVarsDom)

  case class BlockState(
    renamesBefore: mutable.Map[Variable, Int] = mutable.Map[Variable, Int](),
    renamesAfter: mutable.Map[Variable, Int] = mutable.Map[Variable, Int](),
    var filled: Boolean = false, /* have given local value numbering */
    var completed: Boolean = false, /* have filled and processed all incoming */
    var isPhi: Boolean = false /* begins filled */
  )

  def renameLHS(c: Command, variable: Variable, index: Int) = {
    c match {
      case s: Statement => visit_stmt(StmtRenamer(Map((variable -> index)), Map()), s)
      case j: Jump => visit_jump(StmtRenamer(Map((variable -> index)), Map()), j)
    }
  }

  def renameRHS(c: Command, variable: Variable, index: Int) = {
    c match {
      case s: Statement => visit_stmt(StmtRenamer(Map(), Map((variable -> index))), s)
      case j: Jump => visit_jump(StmtRenamer(Map(), Map((variable -> index))), j)
    }
  }

  def appendAssign(b: Block, s: LocalAssign) = {
    // maintain call end of lock invariant
    if (b.statements.size > 0 && b.statements.last.isInstanceOf[Call]) {
      b.statements.insertBefore(b.statements.last, s)
    } else {
      b.statements.append(s)
    }
  }

  def withDefault(_st: mutable.Map[Block, BlockState])(b: Block) = {
    if _st.contains(b) then _st(b)
    else {
      _st(b) = BlockState()
      _st(b)
    }
  }

  def localProcessBlock(
    state: mutable.Map[Block, BlockState],
    count: mutable.Map[Variable, Int],
    block: Block
  ): Unit = {
    def st(b: Block) = withDefault(state)(b)

    val params = block.parent.formalInParam
    var renames = st(block).renamesBefore.toMap

    for (s <- block.statements) {

      // to handle blocks unreachable from the entry we increment the index of the RHS variable
      // for non-parameter free variables that don't have a rename defined by their predecessor
      val incomingRenames = (freeVarsPos(s) -- params -- renames.keySet).map(v => {
        count(v) = count(v) + 1
        v -> count(v)
      })
      val paramDeps = (params.toSet -- renames.keySet).map(_ -> 0) // also add missing params to analysis state
      st(block).renamesBefore.addAll(incomingRenames ++ paramDeps)
      renames = renames ++ incomingRenames ++ paramDeps

      // perform rename
      visit_stmt(StmtRenamer(Map(), renames), s)
      s match {
        case d: Assign => {
          val vars = d.assignees
          for (lhs <- vars) {
            count(lhs) = count(lhs) + 1
            renameLHS(d, lhs, count(lhs))
            renames = renames + (lhs -> count(lhs))
          }
        }
        case _ => ()
      }
    }

    visit_jump(StmtRenamer(Map(), renames), block.jump)
    st(block).renamesAfter.addAll(renames)
    st(block).filled = true

  }

  def applyTransform(p: Program): Map[String, result] = {
    p.procedures
      .map(proc => {
        proc.name -> applyTransform(proc)
      })
      .toMap
  }

  def createBlockBetween(b1: Block, b2: Block, label: String = "phi"): Block = {
    b1.createBlockBetween(b2, label)
  }

  def fixPredecessors(
    _st: mutable.Map[Block, BlockState],
    count: mutable.Map[Variable, Int],
    liveBefore: mutable.Map[Block, Set[Variable]],
    block: Block
  ) = {
    def state(b: Block) = withDefault(_st)(b)

    val preds = block.prevBlocks.toList
    val toJoin = preds.filter(state(_).filled)
    debugAssert(!(toJoin.isEmpty && preds.nonEmpty), s"should always have at least one processed predecessor ${preds}")

    {
      val definedVars = toJoin.flatMap(state(_).renamesAfter.keySet).toSet.intersect(liveBefore(block))
      val toUnify = definedVars
        .map(v => v -> toJoin.map(state(_).renamesAfter.get(v).getOrElse(0)))
        .filter((v, rns) => {
          rns.toList match {
            case Nil => false
            case h :: Nil => false
            // if there is no renaming such that all the incoming renames agree
            // then we create a new copy
            case h :: tl =>
              tl.foldLeft(Some(h): Option[Int])((acc: Option[Int], rn: Int) =>
                acc match {
                  case Some(v) if v == rn => Some(v)
                  case _ => None
                }
              ).isEmpty
          }
        })

      if (toUnify.nonEmpty) {
        val blocks = toJoin.map(b => b -> createBlockBetween(b, block, "phi_back")).toMap
        for (v <- toUnify.map(_._1)) {
          count(v) = count(v) + 1
          // new index for new copy of v (definition added to all incoming edges)

          for (b <- toJoin) {
            val nb = blocks(b)
            debugAssert(state(b).filled)
            state(nb).renamesBefore.addAll(state(b).renamesAfter)

            val assign = LocalAssign(v, v, Some("phiback"))
            state(nb).renamesAfter(v) = count(v)
            appendAssign(nb, assign)
            renameLHS(assign, v, state(nb).renamesAfter(v))
            renameRHS(assign, v, state(nb).renamesBefore.get(v).getOrElse(0))
            state(block).renamesBefore.addAll(state(nb).renamesAfter)
          }

        }
      } else {
        // all our incoming are equal, or we have only one predecessor etc
        for (b <- toJoin) {
          state(block).renamesBefore.addAll(state(b).renamesAfter)
        }
      }
    }
    // set completed
    if (toJoin.size == preds.size) {
      state(block).completed = true
    } else {
      state(block).completed = false
    }

  }

  def fixSuccessors(
    _st: mutable.Map[Block, BlockState],
    count: mutable.Map[Variable, Int],
    liveBefore: mutable.Map[Block, Set[Variable]],
    liveAfter: mutable.Map[Block, Set[Variable]],
    block: Block
  ) = {
    def state(b: Block) = withDefault(_st)(b)

    val next = block.nextBlocks.toList
    // any next not completed
    val anyNextPrevNotFilled = next.exists(_.prevBlocks.exists(b => !state(b).filled))
    val incompleteSuccessor = next.exists(b => !(state(b).completed))
    debugAssert(anyNextPrevNotFilled == incompleteSuccessor)
    for (b <- next) {
      val definedVars = state(block).renamesAfter.keySet.intersect(liveAfter(block))

      if (definedVars.size > 0) {
        val nb = createBlockBetween(block, b, "phi")

        state(nb).renamesBefore.addAll(state(block).renamesAfter)
        if (state(b).filled) {
          // if filled we have chosen an incoming rename
          state(nb).renamesAfter.addAll(state(b).renamesBefore)
        }

        for (v <- definedVars) {
          if (!state(nb).renamesAfter.contains(v)) {
            // pre-emptively create a copy for this branch entry, e.g. to create a new copy for loop headers
            count(v) = count(v) + 1
            state(nb).renamesAfter(v) = count(v)
          }
          if (state(nb).renamesBefore(v) != state(nb).renamesAfter(v)) {
            val assign = LocalAssign(v, v, phiAssignLabel)
            appendAssign(nb, assign)
            renameLHS(assign, v, state(nb).renamesAfter(v))
            renameRHS(assign, v, state(nb).renamesBefore(v))
          }
        }
        state(nb).filled = true
        state(nb).isPhi = true
        liveBefore(nb) = liveAfter(block)
        liveAfter(nb) = liveBefore(b)
      }
    }
  }

  def visitBlock(
    _st: mutable.Map[Block, BlockState],
    count: mutable.Map[Variable, Int],
    liveBefore: mutable.Map[Block, Set[Variable]],
    liveAfter: mutable.Map[Block, Set[Variable]],
    block: Block
  ) = {

    /** VisitBlock:
      *
      *   1. for all complete incoming
      *      - if there is an incoming rename that is not equal across the predecessors
      *      - add back phi block to each incoming edge
      *      - create a fresh copy of each non-uniform renamed variable
      *      - add copies to each phi block unify the incoming rename with the nominated new rename 2. add local value
      *        numbering to this block 3. if all predecessors are filled, mark this complete, otherwise mark this
      *        filled. 4. for all successors
      *   - if marked filled, add phi block to unify our outgoing with its incoming
      *   - if > 1 total predecessors for each unmarked , add phi block to nominate a new copy 5. for all successors, if
      *     all predecessors are now filled, mark complete
      */

    def state(b: Block) = withDefault(_st)(b)

    // add copies on incoming edges to make the outgoing rename of all filled precesessors
    // the same, and define `block`'s incoming rename
    fixPredecessors(_st, count, liveBefore, block)

    // apply local value numbering to `block`
    var seenBefore = true
    if (!(state(block).filled)) {
      localProcessBlock(_st, count, block)
      state(block).filled = true
      debugAssert(state(block).filled)
      seenBefore = false
    }

    // mark successors complete which are completed as a result of processing `block`
    for (b <- block.nextBlocks) {
      if (b.prevBlocks.forall(state(_).filled)) {
        state(b).completed = true
      }
    }

    // add outgoing copies for e.g. loop headers
    fixSuccessors(_st, count, liveBefore, liveAfter, block)
  }

  type result = Map[String, (Map[Variable, Variable], Map[Variable, Variable])]

  def stToResult(_st: mutable.Map[Block, BlockState]): result = {
    _st.toMap.map((bl: Block, bs: BlockState) => {
      bl.label -> (
        bs.renamesBefore.toMap.map { case (v, idx) =>
          (v, visit_rvar(StmtRenamer(Map(), Map(v -> idx)), v))
        },
        bs.renamesAfter.toMap.map { case (v, idx) =>
          (v, visit_rvar(StmtRenamer(Map(), Map(v -> idx)), v))
        }
      )
    })
  }

  def applyTransform(p: Procedure): result = {
    val _st = mutable.Map[Block, BlockState]()
    // ensure order is defined
    ir.transforms.reversePostOrder(p)

    val (liveBeforeIn, liveAfterIn) = liveVarsSolver.solveProc(p, backwards = true)
    val liveBefore = mutable.Map.from(liveBeforeIn)
    val liveAfter = mutable.Map.from(liveAfterIn)

    for (b <- p.blocks.filterNot(liveBeforeIn.contains)) {
      liveBefore(b) = liveVarsDom.bot
    }
    for (b <- p.blocks.filterNot(liveAfterIn.contains)) {
      liveAfter(b) = liveVarsDom.bot
    }

    val worklist = mutable.PriorityQueue[Block]()(Ordering.by(b => b.rpoOrder))
    worklist.addAll(p.blocks)
    var seen = Set[Block]()
    val count = mutable.Map[Variable, Int]().withDefaultValue(0)

    while (worklist.nonEmpty) {
      while (worklist.nonEmpty) {
        val block = worklist.dequeue
        debugAssert(worklist.headOption.map(_.rpoOrder < block.rpoOrder).getOrElse(true))

        visitBlock(_st, count, liveBefore, liveAfter, block)
      }
    }

    // fix up rpo index of added phi blocks

    val maxIndex = (Seq(0) ++ freeVarsPos(p).collect {
      case l: LocalVar if l.index != 0 => l.index
    }).max
    p.ssaCount = maxIndex + 1

    // combine phis

    for (b <- p.blocks) {
      if (_st(b).isPhi) {
        val assignments = {
          val ns = SimulAssign(b.statements.map {
            case l: LocalAssign => (l.lhs, l.rhs)
            case _ => throw Exception("Expect phi block to only contain assignments")
          }.toVector)
          b.statements.clear()
          b.statements.prepend(ns)
        }

      }
    }

    reversePostOrder(p)

    stToResult(_st)
  }

}

class StmtRenamer(renamesL: Map[Variable, Int] = Map(), renames: Map[Variable, Int] = Map()) extends CILVisitor {

  private def addIndex(v: Variable, idx: Int) = {
    debugAssert(idx != -1)
    v match {
      case Register(n, sz) => {
        throw Exception("Should not SSA registers")
        Register(n + "_" + idx, sz)
      }
      case GlobalVar(n, t) => {
        throw Exception("Should not SSA globals")
        GlobalVar(n + "_" + idx, t)
      }
      case v: LocalVar => LocalVar(v.varName, v.irType, idx)
    }
  }

  override def vrvar(v: Variable) = v match {
    case v if renames.contains(v) && renames(v) != -1 => ChangeTo(addIndex(v, renames(v)))
    case _ => DoChildren()
  }

  override def vlvar(v: Variable) = v match {
    case v if renamesL.contains(v) && renamesL(v) != -1 => ChangeTo(addIndex(v, renamesL(v)))
    case _ => DoChildren()
  }
}

def rdDSAProperty(p: Procedure): Boolean = {
  /*
   * Check the DSA property using a reaching definitions analysis.
   * DSA Property: Every use of a variable v has every definition of v as a reaching definition
   *  / no strict subset of defintions of v defines any use of v, forall v.
   */
  val defs: Map[Variable, Set[Assign]] = p
    .flatMap {
      // case a: SingleAssign => Seq((a.lhs, (a: Assign)))
      case a: Assign => a.assignees.map((l: Variable) => (l, a)).toSeq
      case _ => Seq()
    }
    .groupBy(_._1)
    .map((v, vs) => (v, vs.map(_._2).toSet))

  Logger.debug(s"Reaching defs ${p.name}")
  val reachingDefs = basicReachingDefs(p)
  Logger.debug(s"Reaching defs ${p.name} DONE")

  class CheckDSAProperty(defs: Map[Variable, Set[Assign]], reaching: Map[Command, Map[Variable, Set[Assign]]])
      extends CILVisitor {
    var passed = true
    var stmt: Command = null
    val violations = mutable.HashSet[(Command, Variable)]()

    override def vrvar(v: Variable) = {
      val allDefs = defs.get(v).toSet.flatten
      val reachDefs = reachingDefs.get(stmt).flatMap(_.get(v)).toSet.flatten

      val check = allDefs == reachDefs
      if (!check) {
        val vil = (stmt, v)
        if (!violations.contains(vil)) {
          violations.add(vil)
          // Logger.error(s"DSA Property violated on $v at $stmt @ ${stmt.parent.parent.name}::${stmt.parent.label}\n\t ${allDefs.diff(reachDefs)} defs not reached")
          Logger.error(
            s"DSA Property violated on $v at $stmt @ ${stmt.parent.parent.name}::${stmt.parent.label}\n\t ${allDefs
                .diff(reachDefs)} defs not reached\n\t${reachDefs}"
          )
        }
      }
      passed = passed && check
      SkipChildren()
    }
    override def vstmt(v: Statement) = {
      stmt = v
      DoChildren()
    }
    override def vjump(j: Jump) = {
      stmt = j
      DoChildren()
    }

  }

  val vis = CheckDSAProperty(defs, reachingDefs)
  visit_proc(vis, p)
  if (vis.passed) {
    Logger.debug(s"${p.name} DSA check OK")
  }
  vis.passed
}

object DSAPropCheck {
  // check the property that no strict subset of definitions dominates a use
  //
  // This attempts to generate an SMT proof of this, by encoding the reaching definitions in SMT2
  // Likely this does not work.

  def getUses(s: CFGPosition): Set[Variable] = {
    s match {
      case a: LocalAssign => a.rhs.variables
      case a: MemoryStore => a.index.variables ++ a.value.variables
      case a: MemoryLoad => a.index.variables
      case a: DirectCall => a.actualParams.flatMap(_._2.variables).toSet
      case a: Return => a.outParams.flatMap(_._2.variables).toSet
      case a: IndirectCall => Set(a.target)
      case a: Assert => a.body.variables
      case a: Assume => a.body.variables
      case _ => Set()
    }
  }

  def getDefinitions(s: CFGPosition): Set[Variable] = {
    s match {
      case a: Assign => a.assignees
      case _ => Set()
    }
  }

  def emitProof(proc: Procedure) = {

    var fresh = 0
    val vartoIdx = mutable.Map[Variable, Int]()
    val nodeToIdx = mutable.Map[CFGPosition, Int]()

    val blockcfg = proc.blocks.flatMap(b => {
      val pred = IRWalk.lastInBlock(b)
      b.statements.map(s => (s, s.successor)) ++
        b.nextBlocks
          .map(IRWalk.firstInBlock)
          .map(succ => {
            (pred, succ)
          })
    })

    def getVarIdx(n: Variable): Int = {
      if (vartoIdx.contains(n)) {
        vartoIdx(n)
      } else {
        fresh += 1
        vartoIdx(n) = fresh
        vartoIdx(n)
      }
    }

    def getGraphNode(p: CFGPosition): Int = {
      // val idx = getVarIdx(v) // put in map
      val n = p
      if (nodeToIdx.contains(n)) {
        nodeToIdx(n)
      } else {
        fresh += 1
        nodeToIdx(n) = fresh
        nodeToIdx(n)
      }
    }

    def defTruePredicate(name: String, args: List[IRType]) =
      list(
        sym("declare-fun"),
        sym(name),
        Sexp.Slist(args.map(i => BasilIRToSMT2.basilTypeToSMTType(i)).toList),
        BasilIRToSMT2.basilTypeToSMTType(BoolType)
      )

    def dominates[T](pred: Sexp[T], succ: Sexp[T]) = {
      list(sym("assert"), list(sym("dominates"), pred, succ))
    }
    def notDominates[T](pred: Sexp[T], succ: Sexp[T]) = {
      list(sym("assert"), list(sym("not"), list(sym("dominates"), pred, succ)))
    }

    val edges = blockcfg.map((p, s) => ((getGraphNode(p)), (getGraphNode(s)))).toSet
    val nodes = edges.flatMap((a, b) => Seq(a, b))

    val doms = nodes.flatMap(nn1 => {
      nodes.flatMap(nn2 => {
        val (n1, n2) = (BasilIRToSMT2.int2smt(nn1), BasilIRToSMT2.int2smt(nn2))
        if (edges.contains((nn1, nn2))) {
          Seq(dominates(n1, n2))
        } else {
          Seq()
          // notDominates(n1, n2)
        }
      })
    })

    val res: List[Sexp[_]] = List(
      defTruePredicate("isUse", List(IntType, IntType)), // node, var
      defTruePredicate("isDef", List(IntType, IntType)), // node, var
      defTruePredicate("dominates", List(IntType, IntType)) // node, node
    ) ++ doms

    val written = res.map(Sexp.print)

    val vars = nodeToIdx.flatMap((p, nodeID) => {
      getUses(p).map(vn => {
        val v = getVarIdx(vn)
        list(sym("assert"), list(sym("isUse"), BasilIRToSMT2.int2smt(nodeID), BasilIRToSMT2.int2smt(v)))
      })
        ++
          getDefinitions(p).map(vn => {
            val v = getVarIdx(vn)
            list(sym("assert"), list(sym("isUse"), BasilIRToSMT2.int2smt(nodeID), BasilIRToSMT2.int2smt(v)))
          })
    })
    val axioms = List(
      "(assert (forall ((x Int) (y Int) (z Int)) (implies (and (dominates x y) (dominates y z)) (dominates x z))))",
      "(assert (not (forall ((n Int) (v Int) (v2 Int)) (and (isDef n v) (isUse n v2)))))",
      // check of dsa property
      """(declare-fun defines (Int Int) Bool)
(assert (forall ((d Int) (u Int) (i Int) (v Int)) (implies (and (isUse u v) (isDef d v) (dominates d u) (not (and (dominates d i) (dominates i u) (isDef i v)))) (defines d u))))
(assert (not (forall ((usenode Int) (variable Int)) 
  (implies  (isUse usenode variable) 
    (forall ((defnode Int))
      (implies (isDef defnode variable)
        (defines defnode usenode)))))))
        """
    )

    util.writeToFile(
      (written ++ vars.map(Sexp.print) ++ axioms).mkString("\n") + "\n(check-sat)",
      s"proofs/${proc.name}-dsa-graph.smt2"
    )
  }

}
