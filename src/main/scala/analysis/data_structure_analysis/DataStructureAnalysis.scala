package analysis.data_structure_analysis

import analysis.*
import ir.*
import specification.{ExternalFunction, SymbolTableEntry}
import util.Counter
import util.assertion.*

import scala.collection.mutable

case class DSAContext(
  sva: Map[Procedure, SymValues[OSet]],
  constraints: Map[Procedure, Set[Constraint]],
  local: Map[Procedure, IntervalGraph],
  bottomUp: Map[Procedure, IntervalGraph],
  topDown: Map[Procedure, IntervalGraph],
  globals: Map[IntervalNode, IntervalNode]
)

/** Data Structure Analysis Performs all phases of DSA and stores the results in member variables local, bottom-up,
  * top-down results in member variables locals, bu and td respectively.
  * @param program
  *   program to be analysed
  * @param symResults
  *   result of symbolic access analysis
  * @param constProp
  * @param globals
  * @param globalOffsets
  * @param externalFunctions
  * @param reachingDefs
  * @param writesTo
  *   mapping from procedures to registers they change
  * @param params
  *   mapping from procedures to their parameters
  */
class DataStructureAnalysis(
  program: Program,
  symResults: Map[CFGPosition, Map[SymbolicAddress, TwoElement]],
  constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
  globals: Set[SymbolTableEntry],
  globalOffsets: Map[BigInt, BigInt],
  externalFunctions: Set[ExternalFunction],
  reachingDefs: Map[CFGPosition, Map[Variable, Set[CFGPosition]]],
  writesTo: Map[Procedure, Set[GlobalVar]],
  params: Map[Procedure, Set[Variable]]
) extends Analysis[Map[Procedure, Graph]] {

  private val counter = Counter()
  given Counter = counter

  val local: mutable.Map[Procedure, Graph] = mutable.Map()
  val bottomUp: mutable.Map[Procedure, Graph] = mutable.Map()
  val topDown: mutable.Map[Procedure, Graph] = mutable.Map()

  private val stackPointer = Register("R31", 64)
  private val returnPointer = Register("R30", 64)
  private val framePointer = Register("R29", 64)

  private val ignoreRegisters: Set[Variable] = Set(stackPointer, returnPointer, framePointer)

  private def findLeaf(procedure: Procedure): Set[Procedure] = {
    val succ = CallGraph.succ(procedure)
    if (succ.isEmpty) {
      Set(procedure)
    } else {
      succ.flatMap(findLeaf)
    }
  }

  private var visited = Set[Procedure]()
  private val queue = mutable.Queue[Procedure]()

  override def analyze(): Map[Procedure, Graph] = {
    var domain: Set[Procedure] = Set(program.mainProcedure)
    val stack: mutable.Stack[Procedure] = mutable.Stack()
    stack.pushAll(program.mainProcedure.calls)

    // calculate the procedures used in the program
    while (stack.nonEmpty) {
      val current = stack.pop()
      domain += current
      stack.pushAll(current.calls.diff(domain))
    }

    // perform local analysis on all procs
    domain.foreach { proc =>
      val dsg = LocalPhase(
        proc,
        symResults,
        constProp,
        globals,
        globalOffsets,
        externalFunctions,
        reachingDefs,
        writesTo,
        params
      ).analyze()

      local.update(proc, dsg)
      bottomUp.update(proc, dsg.cloneSelf())
    }

    val leafNodes = findLeaf(program.mainProcedure)

    leafNodes.foreach { proc =>
      debugAssert(local(proc).callsites.isEmpty)
      visited += proc
      // val preds: Set[Procedure] = CallGraph.pred(proc)
      queue.enqueueAll(CallGraph.pred(proc).diff(visited).intersect(domain))
    }

    // bottom up phase
    while queue.nonEmpty do
      var proc = queue.dequeue()
      while !local.contains(proc) && queue.nonEmpty do proc = queue.dequeue()
      visited += proc
      if local.contains(proc) then
        queue.enqueueAll(CallGraph.pred(proc).diff(visited))
        val buGraph = bottomUp(proc)

        buGraph.callsites.foreach { callSite =>
          val callee = callSite.proc
          val calleeGraph = local(callee) // .cloneSelf()
          debugAssert(buGraph.globalMapping.keySet.equals(calleeGraph.globalMapping.keySet))
          debugAssert(calleeGraph.formals.keySet.diff(ignoreRegisters).equals(callSite.paramCells.keySet))
          calleeGraph.globalMapping.values.foreach { field =>
            val newNode = calleeGraph.find(field.node).node
            newNode.cloneNode(calleeGraph, buGraph)
          }

          calleeGraph.formals.foreach { (variable, slice) =>
            if (!ignoreRegisters.contains(variable)) {
              debugAssert(callSite.paramCells.contains(variable))
              val node = calleeGraph.find(slice).node
              node.cloneNode(calleeGraph, buGraph)
            }
          }

          debugAssert(writesTo(callee).equals(callSite.returnCells.keySet))
          writesTo(callee).foreach { reg =>
            debugAssert(callSite.returnCells.contains(reg))
            val returnCells = calleeGraph.getCells(IRWalk.lastInProc(callee).get, reg).map(calleeGraph.find)
            debugAssert(returnCells.nonEmpty)
            returnCells.foreach { slice =>
              val node = calleeGraph.find(slice).node
              node.cloneNode(calleeGraph, buGraph)
            }
          }

          //          debugAssert(calleeGraph.formals.isEmpty || buGraph.varToCell(begin(callee)).equals(calleeGraph.formals))
          calleeGraph.globalMapping.foreach { case (range: AddressRange, Field(node: Node, offset: BigInt)) =>
            val field = calleeGraph.find(node)
            val res = buGraph.mergeCells(
              buGraph.globalMapping(range).node.getCell(buGraph.globalMapping(range).offset),
              field.node.getCell(field.offset + offset)
            )
            buGraph.handleOverlapping(res)
          }

          if (buGraph.varToCell.contains(callee)) {
            buGraph.varToCell(callee).keys.foreach { variable =>
              if (!ignoreRegisters.contains(variable)) {
                val formal = buGraph.varToCell(callee)(variable)
                val res = buGraph.mergeCells(buGraph.adjust(formal), buGraph.adjust(callSite.paramCells(variable)))
                buGraph.handleOverlapping(res)
              }
            }
          }

          writesTo(callee).foreach { reg =>
            val returnCells = buGraph.getCells(IRWalk.lastInProc(callee).get, reg)
            //              debugAssert(returnCells.nonEmpty)
            val res = returnCells.foldLeft(buGraph.adjust(callSite.returnCells(reg))) { (c, ret) =>
              buGraph.mergeCells(c, buGraph.adjust(ret))
            }
            buGraph.handleOverlapping(res)
          }
        }
        buGraph.collectNodes()
    // bottom up phase finished
    // clone bu graphs to top-down graphs
    domain.foreach { proc =>
      topDown.update(proc, bottomUp(proc).cloneSelf())
    }

    queue.enqueue(program.mainProcedure)
    visited = Set()

    // top-down phase
    while (queue.nonEmpty) {
      val proc = queue.dequeue()
      visited += proc
      queue.enqueueAll(CallGraph.succ(proc).diff(visited))
      val callersGraph = topDown(proc)
      callersGraph.callsites.foreach { callSite =>
        val callee = callSite.proc
        val calleesGraph = topDown(callee)
        debugAssert(callersGraph.globalMapping.keySet.equals(calleesGraph.globalMapping.keySet))

        callersGraph.globalMapping.values.foreach { field =>
          val oldNode = field.node
          val node = callersGraph.find(oldNode).node
          node.cloneNode(callersGraph, calleesGraph)
        }

        callSite.paramCells.values.foreach { slice =>
          val node = callersGraph.find(slice).node
          node.cloneNode(callersGraph, calleesGraph)
        }

        callSite.returnCells.values.foreach { slice =>
          val node = callersGraph.find(slice).node
          node.cloneNode(callersGraph, calleesGraph)
        }

        callersGraph.globalMapping.foreach { case (range: AddressRange, Field(oldNode, internal)) =>
          //              val node = callersGraph
          val field = callersGraph.find(oldNode)
          val res = calleesGraph.mergeCells(
            calleesGraph.globalMapping(range).node.getCell(calleesGraph.globalMapping(range).offset),
            field.node.getCell(field.offset + internal)
          )
          calleesGraph.handleOverlapping(res)
        }

        callSite.paramCells.keySet.foreach { variable =>
          val paramCells = calleesGraph.getCells(callSite.call, variable) // wrong param offset
          val res = paramCells.foldLeft(calleesGraph.adjust(calleesGraph.formals(variable))) { (cell, slice) =>
            calleesGraph.mergeCells(calleesGraph.adjust(slice), cell)
          }
          calleesGraph.handleOverlapping(res)

        }

        if (calleesGraph.varToCell.contains(callSite.call)) {
          calleesGraph.varToCell(callSite.call).foreach { (variable, oldSlice) =>
            val slice = callersGraph.find(oldSlice)
            val returnCells = calleesGraph.getCells(IRWalk.lastInProc(callee).get, variable)
            val res = returnCells.foldLeft(calleesGraph.adjust(slice)) { (c, retCell) =>
              calleesGraph.mergeCells(c, calleesGraph.adjust(retCell))
            }

            calleesGraph.handleOverlapping(res)
          }
        }
      }
      callersGraph.collectNodes()
    }
    topDown.toMap

  }
}
