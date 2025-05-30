package ir.slicer

import ir.*
import ir.transforms.{stripUnreachableFunctions, cleanupBlocks, removeDeadInParams}
import analysis.{Lambda, EdgeFunction, TwoElement}
import util.{SlicerConfig, SlicerLogger, LogLevel, PerformanceTimer}
import scala.collection.mutable

protected type StatementSlice = Set[Variable]
object StatementSlice {
  def apply(): StatementSlice = Set.empty[Variable]
}

class Slicer(program: Program, slicerConfig: SlicerConfig) {
  protected val performanceTimer = PerformanceTimer("Slicer Timer", LogLevel.INFO)

  lazy protected val parsedConfig: Option[(Block, StatementSlice)] = {
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
      var remainingNames = mutable.Set() ++ slicerConfig.initialCriterion

      val worklist = mutable.PriorityQueue[Block]()(Ordering.by(b => -b.rpoOrder))
      worklist.addOne(targetedBlock)

      while (worklist.nonEmpty && remainingNames.nonEmpty) {
        val b = worklist.dequeue

        if (!visited(b.label)) {
          visited.put(b.label, true)

          val blockVars =
            b.statements.flatMap(c => variables(c)).filter(v => remainingNames.contains(v.name)) ++ variables(b.jump)
          detectedVariables.addAll(blockVars)
          remainingNames.subtractAll(blockVars.map(_.name))

          worklist.addAll(IntraProcBlockIRCursor.pred(b))
          worklist.addAll(b.calls.collect { case t if t.returnBlock.isDefined => t.returnBlock.get })

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

  lazy protected val initialCriterion: Map[CFGPosition, StatementSlice] = {
    parsedConfig match {
      case Some(targetedBlock, variables) => Map(targetedBlock.jump -> variables)
      case _ => Map()
    }
  }

  lazy protected val startingNode = {
    parsedConfig match {
      case Some(targetedBlock, _) => targetedBlock.jump
      case _ => IRWalk.lastInProc(program.mainProcedure).getOrElse(program.mainProcedure)
    }
  }

  class Phase1 {
    protected var nop: Option[NOP] = None
    protected def insertNOP: Unit = startingNode match {
      case c: Command =>
        IRWalk.prevCommandInBlock(c) match {
          case Some(d: DirectCall) if d.target.returnBlock.isDefined =>
            nop = Some(NOP())
            c.parent.statements.insertAfter(d, nop.get)
          case _ => ()
        }
      case _ => ()
    }

    protected def removeNOP: Unit = nop match {
      case Some(n) => n.parent.statements.remove(n)
      case _ => ()
    }

    def run(): Map[CFGPosition, Set[Variable]] = {
      SlicerLogger.info("Slicer :: Slicing Criterion Generation - Phase1")
      insertNOP
      val results = SlicerAnalysis(program, startingNode, initialCriterion)
        .analyze()
        .map({ case (n, e) => n -> e.keys.toSet })
      performanceTimer.checkPoint("Finished IDE Analysis")
      removeNOP
      results
    }
  }

  class Phase2(results: Map[CFGPosition, StatementSlice]) {
    val transferFunctions = SlicerTransfers(initialCriterion)

    def procedures = program.procedures.filterNot(_.isExternal.contains(true))

    def transfer(n: CFGPosition): Map[Variable, EdgeFunction[TwoElement]] = {
      val func = transferFunctions.edgesOther(n)
      transferFunctions.restructure(criterion(n).flatMap(v => func(Left(v))).toMap ++ func(Right(Lambda())))
    }

    def criterion(n: CFGPosition): StatementSlice = {
      (n match {
        case c: DirectCall => transfer(c.successor).keys.toSet
        case _ => results.getOrElse(n, Set())
      }) ++ initialCriterion.getOrElse(n, Set())
    }

    private val procedureModifies = mutable.Map[Procedure, Set[Variable]]()
    private val procedureMemoryIndexes = mutable.Map[Procedure, Set[Variable]]()

    def hasCriterionImpact(n: Statement): Boolean = {
      val crit = criterion(n)

      n match {
        case c: DirectCall =>
          c.outParams.values.toSet.exists(crit.contains)
          || procedureModifies
            .getOrElseUpdate(
              c.target,
              c.target.blocks
                .flatMap(_.modifies.collect { case v: Variable => v })
                .toSet
            )
            .exists(crit.contains)
          ||
          procedureMemoryIndexes
            .getOrElseUpdate(
              c.target,
              c.target.blocks
                .flatMap(
                  _.statements
                    .collect {
                      case a: MemoryLoad => transferFunctions.convertMemoryIndex(a.index)
                      case m: MemoryStore => transferFunctions.convertMemoryIndex(m.index)
                    }
                    .flatten
                )
                .toSet
            )
            .exists(crit.contains)
        case _ => {
          val transferred = transfer(n)
          transferred.values.toSet.contains(
            transferFunctions.edgelattice.ConstEdge(transferFunctions.valuelattice.top)
          ) || (transferred.size != crit.size)
        }
      }
    }

    /**
     * {@link Simp#removeDeadInParams(Program)}
     *
     */
    def reduceInParams(): Unit = {
      var modified = false
      assert(invariant.correctCalls(program))

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

    def reduceOutParams(): Unit = {
      var modified = false
      assert(invariant.correctCalls(program))

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

    def run(): Unit = {
      SlicerLogger.info("Slicer :: Reductive Slicing Pass - Phase2")

      SlicerLogger.debug("Slicer - Statement Removal")
      var total = 0
      var removed = 0
      for (procedure <- procedures) {
        for (block <- procedure.blocks) {
          for (statement <- block.statements) {
            if (!hasCriterionImpact(statement)) {
              removed += 1
              statement.parent.statements.remove(statement)
            }
            total += 1
          }
        }
      }
      performanceTimer.checkPoint("Finished Statement Removal")
      SlicerLogger.info(s"Slicer - Removed $removed statements (${total - removed} remaining)")

      SlicerLogger.debug("Slicer - Remove Dead Parameters")
      reduceInParams()
      reduceOutParams()

      SlicerLogger.debug("Slicer - Cleanup Blocks")
      cleanupBlocks(program)

      performanceTimer.checkPoint("Finished IR Cleanup")
    }
  }

  def run(): Unit = {
    SlicerLogger.info("Slicer :: Slicer Start")
    if (parsedConfig.isDefined) {
      val before = program.procedures.size
      stripUnreachableFunctions(program)
      SlicerLogger.info(s"Slicer - Stripping unreachable | Removed ${before - program.procedures.size} functions (${program.procedures.size} remaining)")

      val results = Phase1().run()


      Phase2(results).run()

      performanceTimer.checkPoint("Finished Slicer")
    } else {
      SlicerLogger.error("Skipping Slicer")
    }
  }
}
