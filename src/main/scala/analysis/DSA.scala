package analysis

import ir.{BitVecLiteral, BitVecType, CFGPosition, CallGraph, Procedure, Program, Register, Variable, computeDomain, IRWalk}
import specification.{ExternalFunction, SpecGlobal, SymbolTableEntry}

import scala.collection.mutable

/**
 * Data Structure Analysis
 * Performs all phases of DSA and stores the results in member variables
 * local, bottom-up, top-down results in member variables locals, bu and td respectively.
 * @param program program to be analysed
 * @param symResults result of symbolic access analysis
 * @param constProp
 * @param globals
 * @param globalOffsets
 * @param externalFunctions
 * @param reachingDefs
 * @param writesTo mapping from procedures to registers they change
 * @param params mapping from procedures to their parameters
 */
class DSA(program: Program,
            symResults: Map[CFGPosition, Map[SymbolicAddress, TwoElement]],
            constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
            globals: Set[SymbolTableEntry], globalOffsets: Map[BigInt, BigInt],
            externalFunctions: Set[ExternalFunction],
            reachingDefs: Map[CFGPosition, Map[Variable, Set[CFGPosition]]],
            writesTo: Map[Procedure, Set[Register]],
            params: Map[Procedure, Set[Variable]]
         ) extends Analysis[Map[Procedure, DSG]] {

  val locals: mutable.Map[Procedure, DSG] = mutable.Map()
  val bu: mutable.Map[Procedure, DSG] = mutable.Map()
  val td: mutable.Map[Procedure, DSG] = mutable.Map()

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

  override def analyze(): Map[Procedure, DSG] = {
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
      val dsg = LocalDSA(proc, symResults, constProp, globals, globalOffsets, externalFunctions, reachingDefs, writesTo, params).analyze()

      locals.update(proc, dsg)
      bu.update(proc, dsg.cloneSelf())
    }

    val leafNodes = findLeaf(program.mainProcedure)

    leafNodes.foreach { proc =>
      assert(locals(proc).callsites.isEmpty)
      visited += proc
      //val preds: Set[Procedure] = CallGraph.pred(proc)
      queue.enqueueAll(CallGraph.pred(proc).diff(visited).intersect(domain))
    }

    // bottom up phase
    while queue.nonEmpty do
      var proc = queue.dequeue()
      while !locals.contains(proc) && queue.nonEmpty do proc = queue.dequeue()
      visited += proc
      if locals.contains(proc) then
        queue.enqueueAll(CallGraph.pred(proc).diff(visited))
        val buGraph = bu(proc)

        buGraph.callsites.foreach { callSite =>
          val callee = callSite.proc
          val calleeGraph = locals(callee) //.cloneSelf()
          assert(buGraph.globalMapping.keySet.equals(calleeGraph.globalMapping.keySet))
          assert(calleeGraph.formals.keySet.diff(ignoreRegisters).equals(callSite.paramCells.keySet))
          calleeGraph.globalMapping.values.foreach { field =>
            val newNode = calleeGraph.find(field.node).node
            newNode.cloneNode(calleeGraph, buGraph)
          }

          calleeGraph.formals.foreach { (variable, slice) =>
            if (!ignoreRegisters.contains(variable)) {
              assert(callSite.paramCells.contains(variable))
              val node = calleeGraph.find(slice).node
              node.cloneNode(calleeGraph, buGraph)
            }
          }

          assert(writesTo(callee).equals(callSite.returnCells.keySet))
          writesTo(callee).foreach { reg =>
            assert(callSite.returnCells.contains(reg))
            val returnCells = calleeGraph.getCells(IRWalk.lastInProc(callee).get, reg).map(calleeGraph.find)
            assert(returnCells.nonEmpty)
            returnCells.foreach { slice =>
              val node = calleeGraph.find(slice).node
              node.cloneNode(calleeGraph, buGraph)
            }
          }

          //          assert(calleeGraph.formals.isEmpty || buGraph.varToCell(begin(callee)).equals(calleeGraph.formals))
          calleeGraph.globalMapping.foreach {
            case (range: AddressRange, Field(node: DSN, offset: BigInt)) =>
              val field = calleeGraph.find(node)
              buGraph.mergeCells(
                buGraph.globalMapping(range).node.getCell(buGraph.globalMapping(range).offset),
                field.node.getCell(field.offset + offset)
              )
          }

          if (buGraph.varToCell.contains(callee)) {
            buGraph.varToCell(callee).keys.foreach { variable =>
              if (!ignoreRegisters.contains(variable)) {
                val formal = buGraph.varToCell(callee)(variable)
                buGraph.mergeCells(buGraph.adjust(formal), buGraph.adjust(callSite.paramCells(variable)))
              }
            }
          }

          writesTo(callee).foreach { reg =>
            val returnCells = buGraph.getCells(IRWalk.lastInProc(callee).get, reg)
            //              assert(returnCells.nonEmpty)
            returnCells.foldLeft(buGraph.adjust(callSite.returnCells(reg))) { (c, ret) =>
              buGraph.mergeCells(c, buGraph.adjust(ret))
            }
          }
        }
        buGraph.collectNodes()
    // bottom up phase finished
    // clone bu graphs to top-down graphs
    domain.foreach { proc =>
      td.update(proc, bu(proc).cloneSelf())
    }

    queue.enqueue(program.mainProcedure)
    visited = Set()

    // top-down phase
    while (queue.nonEmpty) {
      val proc = queue.dequeue()
      visited += proc
      queue.enqueueAll(CallGraph.succ(proc).diff(visited))
      val callersGraph = td(proc)
      callersGraph.callsites.foreach { callSite =>
        val callee = callSite.proc
        val calleesGraph = td(callee)
        assert(callersGraph.globalMapping.keySet.equals(calleesGraph.globalMapping.keySet))

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
          node.cloneNode(callersGraph, callersGraph)
        }

        callersGraph.globalMapping.foreach { case (range: AddressRange, Field(oldNode, internal)) =>
          //              val node = callersGraph
          val field = callersGraph.find(oldNode)
          calleesGraph.mergeCells(
            calleesGraph.globalMapping(range).node.getCell(calleesGraph.globalMapping(range).offset),
            field.node.getCell(field.offset + internal)
          )
        }

        callSite.paramCells.keySet.foreach { variable =>
          val paramCells = calleesGraph.getCells(callSite.call, variable) // wrong param offset
          paramCells.foldLeft(calleesGraph.adjust(calleesGraph.formals(variable))) {
            (cell, slice) => calleesGraph.mergeCells(calleesGraph.adjust(slice), cell)
          }
        }

        if (calleesGraph.varToCell.contains(callSite.call)) {
          calleesGraph.varToCell(callSite.call).foreach { (variable, oldSlice) =>
            val slice = callersGraph.find(oldSlice)
            val returnCells = calleesGraph.getCells(IRWalk.lastInProc(callee).get, variable)
            returnCells.foldLeft(calleesGraph.adjust(slice)) {
              (c, retCell) => calleesGraph.mergeCells(c, calleesGraph.adjust(retCell))
            }
          }
        }
      }
      callersGraph.collectNodes()
    }
    td.toMap

  }
}
