package ir.transforms

import analysis.solvers.BackwardIDESolver
import analysis.{
  BackwardIDEAnalysis,
  BackwardIDETransferFunctions,
  EdgeFunction,
  EdgeFunctionLattice,
  Lambda,
  TwoElement,
  TwoElementLattice,
  TwoElementTop
}
import ir.*
import ir.eval.evaluateExpr
import ir.transforms.{cleanupBlocks, stripUnreachableFunctions}
import util.{LogLevel, PerformanceTimer, SlicerConfig, SlicerLogger}

import scala.collection.mutable

/**
 * 2 phase IR program slicer. Destructively removes statement from program.
 * Phase 1 - Criterion Generation
 * Phase 2 - IR Reduction
 *
 * @param program IR program to slice.
 * @param slicerConfig Slicing config to slice wrt.
 */
class Slicer(program: Program, slicerConfig: SlicerConfig) {
  protected val performanceTimer: PerformanceTimer = PerformanceTimer("Slicer Timer", LogLevel.INFO)

  /* Parses slicer config into a valid starting criterion and node */
  lazy protected val parsedConfig: Option[(Block, Set[Variable])] = {
    def variables(n: Command): Set[Variable] = {
      n match {
        case a: LocalAssign => a.lhs.variables ++ a.rhs.variables
        case a: MemoryAssign => a.lhs.variables ++ a.rhs.variables
        case a: MemoryLoad => a.lhs.variables ++ a.index.variables
        case m: MemoryStore => m.index.variables ++ m.value.variables
        case a: Assume => a.body.variables
        case a: Assert => a.body.variables
        case c: DirectCall => c.outParams.values.toSet[Variable] ++ c.actualParams.values.flatMap(_.variables)
        case i: IndirectCall => Set(i.target)
        case r: Return => r.outParams.keys.toSet[Variable] ++ r.outParams.values.flatMap(_.variables)
        case s: (NOP | GoTo | Unreachable) => Set()
      }
    }

    try {
      val targetedBlock = program.labelToBlock(slicerConfig.blockLabel)

      val visited = mutable.Map[String, Boolean]().withDefaultValue(false)

      val detectedVariables = mutable.Set[Variable]()
      val remainingNames = mutable.Set() ++ slicerConfig.initialCriterion.filter(_.nonEmpty)

      val worklist = mutable.PriorityQueue[Block]()(Ordering.by(b => -b.rpoOrder))
      worklist.addOne(targetedBlock)

      while (worklist.nonEmpty && remainingNames.nonEmpty) {
        val b = worklist.dequeue

        if (!visited(b.label)) {
          visited.put(b.label, true)

          val blockVars =
            (b.statements.flatMap(c => variables(c)) ++ variables(b.jump)).filter(v => remainingNames.contains(v.name))
          detectedVariables.addAll(blockVars)
          remainingNames.subtractAll(blockVars.map(_.name))

          worklist.addAll(IntraProcBlockIRCursor.pred(b))
          worklist.addAll(b.calls.collect { case p if p.returnBlock.isDefined => p.returnBlock.get })

          // Reached entry of program from target without finding variables. Re-loop from main return.
          if (worklist.isEmpty && remainingNames.nonEmpty) {
            program.mainProcedure.returnBlock match {
              case Some(block) => worklist.addOne(block)
              case None => ()
            }
          }
        }
      }

      if (remainingNames.isEmpty) {
        Some((targetedBlock, detectedVariables.toSet))
      } else {
        SlicerLogger.error(s"Invalid criterion variables. Could not find variables: ${remainingNames.mkString(", ")}")
        None
      }

    } catch {
      case u: NoSuchElementException => {
        SlicerLogger.error(s"Invalid criterion block: ${slicerConfig.blockLabel} does not exist")
        None
      }
    }
  }

  /* Initial slicing criterion to be transformed by analysis */
  lazy protected val initialCriterion: Map[CFGPosition, Set[Variable]] = {
    parsedConfig match {
      case Some(targetedBlock, variables) => Map(targetedBlock.jump -> variables)
      case _ => Map()
    }
  }

  /* Node to begin analysis from */
  lazy protected val startingNode: CFGPosition = {
    parsedConfig match {
      case Some(targetedBlock, _) => targetedBlock.jump
      case _ => IRWalk.lastInProc(program.mainProcedure).getOrElse(program.mainProcedure)
    }
  }

  /**
   * Criterion generation phase of slice.
   *
   * Transforms initial slicing criterion using IDE Solver transfers.
   * Each program statement transforms the criterion to build final result.
   */
  class Phase1 {
    protected var nop: Option[NOP] = None

    protected def insertNOP(): Unit = startingNode match {
      case c: Command =>
        IRWalk.prevCommandInBlock(c) match {
          case Some(d: DirectCall) if d.target.returnBlock.isDefined =>
            nop = Some(NOP())
            c.parent.statements.insertAfter(d, nop.get)
          case _ => ()
        }
      case _ => ()
    }

    protected def removeNOP(): Unit = nop match {
      case Some(n) => n.parent.statements.remove(n)
      case _ => ()
    }

    /**
     * Runs phase 1 to generate criterion.
     *
     * @return A mapping of CFGPositions to the slicing criterion for said position.
     */
    def run(): Map[CFGPosition, Set[Variable]] = {
      SlicerLogger.info("Slicer :: Slicing Criterion Generation - Phase1")
      insertNOP()
      val results = SlicerAnalysis(program, startingNode, initialCriterion)
        .analyze()
        .map({ case (n, e) => n -> e.keys.toSet })
      SlicerLogger.debug(s"Slicer - Analysed ${results.size} CFG Nodes")
      performanceTimer.checkPoint("Finished IDE Analysis")
      removeNOP()
      results
    }
  }

  /**
   * IR reduction phase. Strips away IR components that are outside the criterion domain.
   *
   * @param results Phase 1 results of CFGPositions to their slicing criterion.
   */
  class Phase2(results: Map[CFGPosition, Set[Variable]]) {
    val transferFunctions: SlicerTransfers = SlicerTransfers(initialCriterion)

    // Cache of procedure global/memory modifications
    private val procedureModifies = mutable.Map[Procedure, Set[Variable]]()
    private val procedureMemoryIndexes = mutable.Map[Procedure, Set[Variable]]()

    private def procedures = program.procedures.filterNot(_.isExternal.contains(true))

    /* Generates transferred criterion for a given position -- transforms initial criterion based on statement. */
    protected def transfer(n: CFGPosition): Map[Variable, EdgeFunction[TwoElement]] = {
      val func = transferFunctions.edgesOther(n)
      (criterion(n).flatMap(v => func(Left(v))).toMap ++ func(Right(Lambda()))).collect { case (Left(k), v) => k -> v }
    }

    /* Slicing criterion for a given position */
    protected def criterion(n: CFGPosition): Set[Variable] = {
      (n match {
        case c: DirectCall => transfer(c.successor).keys.toSet
        case _ => results.getOrElse(n, Set())
      }) ++ initialCriterion.getOrElse(n, Set())
    }

    /* Determines if a given program statement has an impact on the criterion */
    protected def hasCriterionImpact(n: Statement): Boolean = {
      val crit = criterion(n)
      n match {
        case c: DirectCall =>
          // Iff all out parameters impact criterion.
          c.outParams.values.toSet.exists(crit.contains)
          // Iff the criterion was directly modified by call.
          || !crit.equals(results.getOrElse(c, Set()))
          // Iff call modifies global variables in criterion.
          || procedureModifies
            .getOrElseUpdate(
              c.target,
              c.target.blocks
                .flatMap(_.modifies.collect { case v: Variable => v })
                .toSet
            )
            .exists(crit.contains)
          // Iff call stores to memory index in criterion.
          || procedureMemoryIndexes
            .getOrElseUpdate(
              c.target,
              c.target.blocks
                .flatMap(
                  _.statements.collect { case m: MemoryStore => transferFunctions.convertMemoryIndex(m.index) }.flatten
                )
                .toSet
            )
            .exists(crit.filter(_.isInstanceOf[Global]).contains)
        case _ => {
          val transferred = transfer(n)
          transferred.values.toSet.contains(
            transferFunctions.edgelattice.ConstEdge(transferFunctions.valuelattice.top)
          ) || !crit.equals(transferred.keys)
        }
      }
    }

    /**
     * Removes procedure in parameters whose value does not modify impact the criterion for all call contexts.
     *
     * Based on: [[ir.transforms.removeDeadInParams]]
     */
    def reduceInParams(): Unit = {
      var modified = false

      for (procedure <- procedures.filter(_.entryBlock.isDefined)) {
        val unused = procedure.formalInParam.filterNot(criterion(procedure).contains(_))

        for (unusedFormalInParam <- unused) {
          modified = true
          procedure.formalInParam.remove(unusedFormalInParam)

          for (call <- procedure.incomingCalls()) {
            call.actualParams = call.actualParams.removed(unusedFormalInParam)
          }
        }
      }
      if (modified) assert(invariant.correctCalls(program))
    }

    /* Removes procedure out parameters whose value does not modify impact the criterion for all call contexts. */
    def reduceOutParams(): Unit = {
      var modified = false

      for (returnBlock <- procedures.flatMap(_.returnBlock)) {
        val procedure = returnBlock.parent

        val unused = procedure.formalOutParam.filterNot(criterion(returnBlock.jump))

        for (unusedFormalOutParam <- unused) {
          modified = true
          procedure.formalOutParam.remove(unusedFormalOutParam)

          returnBlock.jump match {
            case r: Return => r.outParams = r.outParams.removed(unusedFormalOutParam)
            case _ => ???
          }

          for (call <- procedure.incomingCalls()) {
            call.outParams = call.outParams.removed(unusedFormalOutParam)
          }
        }
      }
      if (modified) assert(invariant.correctCalls(program))
    }

    /* Runs phase 2 reduction */
    def run(): Unit = {
      SlicerLogger.info("Slicer :: Reductive Slicing Pass - Phase2")

      SlicerLogger.debug("Slicer - Statement Removal")
      var total = 0
      var removed = 0
      for (procedure <- procedures) {
        if (!program.mainProcedure.equals(procedure) && !results.contains(procedure)) {
          // Procedure was not analysed therefore can be removed.
          program.removeProcedure(procedure)
        } else {
          for (block <- procedure.blocks) {
            for (statement <- block.statements) {
              if (!hasCriterionImpact(statement)) {
                // If statement has no impact on the criterion it can be safely removed.
                removed += 1
                statement.parent.statements.remove(statement)
              }
              total += 1
            }
          }
        }
      }
      performanceTimer.checkPoint("Finished Statement Removal")
      SlicerLogger.info(s"Slicer - Removed $removed statements (${total - removed} remaining)")

      // Remove unused in and out parameters.
      SlicerLogger.debug("Slicer - Remove Dead Parameters")
      assert(invariant.correctCalls(program))
      reduceInParams()
      reduceOutParams()

      // Reduce and cleanup empty blocks.
      SlicerLogger.debug("Slicer - Cleanup Blocks")
      cleanupBlocks(program)

      // Ensure new IR structural correctness.
      SlicerLogger.debug("Slicer - Invariant Checking")
      assert(invariant.singleCallBlockEnd(program))
      assert(invariant.cfgCorrect(program))
      assert(invariant.blocksUniqueToEachProcedure(program))
      assert(invariant.procEntryNoIncoming(program))

      performanceTimer.checkPoint("Finished IR Cleanup")
    }
  }

  /* Run program slicer to reduce program into slice */
  def run(): Unit = {
    SlicerLogger.info("Slicer :: Slicer Start")
    if (parsedConfig.isDefined) {
      val before = program.procedures.size
      stripUnreachableFunctions(program)
      SlicerLogger.info(
        s"Slicer - Stripping unreachable | Removed ${before - program.procedures.size} functions (${program.procedures.size} remaining)"
      )

      val results = Phase1().run()
      Phase2(results).run()

      performanceTimer.checkPoint("Finished Slicer")
    } else {
      SlicerLogger.error("Skipping Slicer")
    }
  }
}

/**
 * IDE transfer functions outlining criterion modification across IDE edges.
 */
trait SlicerTransferFunctions(slicingCriterion: Map[CFGPosition, Set[Variable]])
    extends BackwardIDETransferFunctions[Variable, TwoElement, TwoElementLattice] {

  val valuelattice = TwoElementLattice()
  val edgelattice = EdgeFunctionLattice(valuelattice)
  import edgelattice.{ConstEdge, IdEdge}

  private def fold(variables: Iterable[Variable]): Map[DL, EdgeFunction[TwoElement]] = {
    variables.map(v => Left(v) -> ConstEdge(TwoElementTop)).toMap
  }

  /**
   * Flow from procedure call point to return of called procedure.
   * Converts criterion element from actual out -> formal out parameter.
   * Prevents local variables but allows global flow.
   */
  def edgesCallToEntry(call: Command, entry: Return)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    d match {
      case Left(value) => {
        val params = IRWalk.prevCommandInBlock(call) match {
          case Some(command) => {
            command match {
              case c: DirectCall => c.outParams
              case i: IndirectCall => Map()
              case _ => ???
            }
          }
          case None => ???
        }

        if params.values.toSet.contains(value)
        then fold(params.filter(_._2 == value).keys)
        else
          value match {
            case g: Global => Map(d -> IdEdge())
            case _ => Map()
          }
      }
      case Right(_) => Map(d -> IdEdge())
    }
  }

  /**
   * Flow from top of procedure back to after call point.
   * Converts criterion element from formal in -> actual in parameter.
   * Prevents local variable flow but allows global flow.
   */
  def edgesExitToAfterCall(exit: Procedure, aftercall: DirectCall)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    d match {
      case Left(value: LocalVar) if aftercall.actualParams.contains(value) =>
        fold(aftercall.actualParams(value).variables)
      case Left(_: LocalVar) => Map()
      case _ => Map(d -> IdEdge())
    }
  }

  /**
   * Flow across call point without entering called procedure.
   * Directly passes all local variables not in out parameters across.
   */
  def edgesCallToAfterCall(call: Command, aftercall: DirectCall)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    d match {
      case Left(value: LocalVar) if aftercall.outParams.values.toSet.contains(value) => Map()
      case Left(_: LocalVar) => Map(d -> IdEdge())
      case Left(_) => Map()
      case Right(_) => Map(d -> IdEdge())
    }
  }

  /**
   * Intraprocedural criterion transforms. Adds initial slicing criterion when first visiting position if required.
   */
  def edgesOther(n: CFGPosition)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    val transferEdge = intraTransferFunctions(n)
    d match {
      case Left(_) => transferEdge(d)
      case Right(_) => transferEdge(d) ++ slicingCriterion.getOrElse(n, Set()).flatMap(v => transferEdge(Left(v))).toMap
    }
  }

  protected def intraTransferFunctions(n: CFGPosition)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    n match {
      case p: Procedure => Map(d -> IdEdge())
      case b: Block => Map(d -> IdEdge())
      case a: LocalAssign => {
        d match {
          case Left(value) if value == a.lhs => fold(a.rhs.variables)
          case _ => Map(d -> IdEdge())
        }
      }
      case a: MemoryAssign => {
        d match {
          case Left(value) if value == a.lhs => fold(a.rhs.variables)
          case _ => Map(d -> IdEdge())
        }
      }
      case a: MemoryLoad => {
        d match {
          case Left(value) if value == a.lhs => fold(convertMemoryIndex(a.index))
          case _ => Map(d -> IdEdge())
        }
      }
      case m: MemoryStore => {
        d match {
          case Left(value) if convertMemoryIndex(m.index).contains(value) => fold(m.value.variables)
          case _ => Map(d -> IdEdge())
        }
      }
      case a: Assume => {
        d match {
          case Left(_) => Map(d -> IdEdge()) ++ fold(a.body.variables)
          case Right(_) => Map(d -> IdEdge())
        }
      }
      case a: Assert => {
        d match {
          case Left(_) => Map(d -> IdEdge()) ++ fold(a.body.variables)
          case Right(_) => Map(d -> IdEdge())
        }
      }
      case c: DirectCall => Map(d -> IdEdge())
      case i: IndirectCall => {
        d match {
          case Left(value: Global) => Map(d -> IdEdge(), Left(i.target) -> ConstEdge(TwoElementTop))
          case _ => Map(d -> IdEdge())
        }
      }
      case n: NOP => Map(d -> IdEdge())
      case g: GoTo => Map(d -> IdEdge())
      case r: Return => {
        d match {
          case Left(value: LocalVar) => {
            r.outParams.get(value) match {
              case Some(returnedValue) => fold(returnedValue.variables)
              case None => Map(d -> IdEdge())
            }
          }
          case _ => Map(d -> IdEdge())
        }
      }
      case u: Unreachable => Map(d -> IdEdge())
    }
  }

  def convertMemoryIndex(index: Expr): Set[Variable] = {
    def convertLiteral(l: Literal): Set[Variable] = {
      l match {
        case bv: BitVecLiteral => Set(Register(bv.toString, bv.size))
        case i: IntLiteral => Set(Register(i.toString, 0))
        case _ => Set()
      }
    }

    index match {
      case v: Variable => Set(v)
      case l: (BitVecLiteral | IntLiteral) => convertLiteral(l)
      case e => {
        evaluateExpr(e) match {
          case Some(l: (BitVecLiteral | IntLiteral)) => convertLiteral(l)
          case _ => index.variables
        }
      }
    }
  }
}

class SlicerTransfers(slicingCriterion: Map[CFGPosition, Set[Variable]])
    extends SlicerTransferFunctions(slicingCriterion)

class SlicerAnalysis(program: Program, startingNode: CFGPosition, slicingCriterion: Map[CFGPosition, Set[Variable]])
    extends BackwardIDESolver[Variable, TwoElement, TwoElementLattice](program)
    with BackwardIDEAnalysis[Variable, TwoElement, TwoElementLattice]
    with SlicerTransferFunctions(slicingCriterion) {
  override def start: CFGPosition = startingNode
}
