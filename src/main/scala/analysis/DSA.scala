package analysis

import ir.{BitVecLiteral, BitVecType, CFGPosition, CallGraph, Procedure, Program, Register, Variable, begin, computeDomain, end}
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
            params:  Map[Procedure, Set[Variable]]
         ) extends Analysis[Map[Procedure, DSG]] {

  val locals : mutable.Map[Procedure, DSG] = mutable.Map()
  val bu: mutable.Map[Procedure, DSG] = mutable.Map()
  val td: mutable.Map[Procedure, DSG] = mutable.Map()

  val stackPointer = Register("R31", 64)
  val returnPointer = Register("R30", 64)
  val framePointer = Register("R29", 64)

  val ignoreRegisters: Set[Variable] = Set(stackPointer, returnPointer, framePointer)

  def findLeaf(procedure: Procedure): Set[Procedure] =
    if CallGraph.succ(procedure).isEmpty then
      Set(procedure)
    else
      CallGraph.succ(procedure).foldLeft(Set[Procedure]()){
        (s, proc) => s ++ findLeaf(proc)
      }


  var visited = Set[Procedure]()
  val queue = mutable.Queue[Procedure]()

  override def analyze(): Map[Procedure, DSG] = {
    var domain : Set[Procedure] = Set(program.mainProcedure)
    val stack : mutable.Stack[Procedure] = mutable.Stack()
    stack.pushAll(program.mainProcedure.calls)

    // calculate the procedures used in the program
    while stack.nonEmpty do
      val current = stack.pop()
      domain += current
      stack.pushAll(current.calls.diff(domain))


    // perform local analysis on all procs
    domain.foreach(
      proc =>
        val dsg = Local(proc, symResults, constProp, globals, globalOffsets, externalFunctions, reachingDefs, writesTo, params).analyze()

        locals.update(proc, dsg)
        bu.update(proc, dsg.cloneSelf())
    )

    Map()
    val leafNodes = findLeaf(program.mainProcedure)

    leafNodes.foreach(
      proc =>
        assert(locals(proc).callsites.isEmpty)
        visited += proc
        val preds : Set[Procedure] = CallGraph.pred(proc)
        queue.enqueueAll(CallGraph.pred(proc).diff(visited).intersect(domain))
    )

    // bottom up phase
    while queue.nonEmpty do
      var proc = queue.dequeue()
      while !locals.contains(proc) && queue.nonEmpty do proc = queue.dequeue()
      visited += proc
      if locals.contains(proc) then
        queue.enqueueAll(CallGraph.pred(proc).diff(visited))
        val buGraph = bu(proc)

        buGraph.callsites.foreach(
          callSite =>
            val callee = callSite.proc
            val calleeGraph = locals(callee) //.cloneSelf()
            assert(buGraph.globalMapping.keySet.equals(calleeGraph.globalMapping.keySet))
            assert(calleeGraph.formals.keySet.diff(ignoreRegisters).equals(callSite.paramCells.keySet))

            calleeGraph.globalMapping.foreach {
              case (range: AddressRange, Field(node, offset)) =>
                val newNode = calleeGraph.find(node).node
                newNode.cloneNode(calleeGraph, buGraph)
            }

            calleeGraph.formals.foreach{
              case (variable: Variable, slice: Slice) if !ignoreRegisters.contains(variable)  =>
                assert(callSite.paramCells.contains(variable))
                val node = calleeGraph.find(slice).node
                node.cloneNode(calleeGraph, buGraph)
              case _ =>
            }

            assert(writesTo(callee).equals(callSite.returnCells.keySet))
            writesTo(callee).foreach(
              reg =>
                assert(callSite.returnCells.contains(reg))
                val returnCells = calleeGraph.getCells(end(callee), reg).map(calleeGraph.find)
                assert(returnCells.nonEmpty)
                returnCells.foreach{
                  case slice: Slice =>
                    val node = calleeGraph.find(slice).node
                    node.cloneNode(calleeGraph, buGraph)
                }
            )

  //          assert(calleeGraph.formals.isEmpty || buGraph.varToCell(begin(callee)).equals(calleeGraph.formals))
            val globalNodes: mutable.Map[Int, DSN] = mutable.Map()
            calleeGraph.globalMapping.foreach {
              case (range: AddressRange, Field(node: DSN, offset: BigInt)) =>
                val field = calleeGraph.find(node)
                buGraph.mergeCells(buGraph.globalMapping(range)._1.getCell(buGraph.globalMapping(range)._2),
                  field.node.getCell(field.offset + offset))
            }

            buGraph.varToCell.getOrElse(begin(callee), Map.empty).foreach{
              case (variable: Variable, formal) if !ignoreRegisters.contains(variable)  =>
                val test = buGraph.mergeCells(buGraph.adjust(formal), buGraph.adjust(callSite.paramCells(variable)))
                test
              case _ =>
            }
            writesTo(callee).foreach(
              reg =>
                val returnCells = buGraph.getCells(end(callee), reg)
                //              assert(returnCells.nonEmpty)
                val result: DSC = returnCells.foldLeft(buGraph.adjust(callSite.returnCells(reg))){
                  //
                  case (c: DSC, ret) =>
                    val test = buGraph.mergeCells(c, buGraph.adjust(ret))
                    test
                }
            )
        )
        buGraph.collectNodes
    // bottom up phase finished
    // clone bu graphs to top-down graphs
    domain.foreach(
      proc =>
        td.update(proc, bu(proc).cloneSelf())
    )

    queue.enqueue(program.mainProcedure)
    visited = Set()


    // top-down phase
    while queue.nonEmpty do
      val proc = queue.dequeue()
      visited += proc
      queue.enqueueAll(CallGraph.succ(proc).diff(visited))
      val callersGraph = td(proc)
      callersGraph.callsites.foreach(
        callSite =>
          val callee = callSite.proc
          val calleesGraph = td(callee)
          assert(callersGraph.globalMapping.keySet.equals(calleesGraph.globalMapping.keySet))

          callersGraph.globalMapping.foreach {
            case (range: AddressRange, Field(oldNode, offset)) =>
              val node = callersGraph.find(oldNode).node
              node.cloneNode(callersGraph, calleesGraph)
          }


          callSite.paramCells.foreach{
            case (variable: Variable, slice: Slice) =>
              val node = callersGraph.find(slice).node
              node.cloneNode(callersGraph, calleesGraph)
          }

          callSite.returnCells.foreach{
            case (variable: Variable, slice: Slice) =>
              val node = callersGraph.find(slice).node
              node.cloneNode(callersGraph, callersGraph)
          }


          callersGraph.globalMapping.foreach {
            case (range: AddressRange, Field(oldNode, internal)) =>
//              val node = callersGraph
              val field = callersGraph.find(oldNode)
              calleesGraph.mergeCells(calleesGraph.globalMapping(range)._1.getCell(calleesGraph.globalMapping(range)._2),
                field.node.getCell(field.offset + internal))
          }

          callSite.paramCells.keySet.foreach(
            variable =>
              val paramCells = calleesGraph.getCells(callSite.call, variable) // wrong param offset
              paramCells.foldLeft(calleesGraph.adjust(calleesGraph.formals(variable))) {
                (cell, slice) =>
                  calleesGraph.mergeCells(calleesGraph.adjust(slice), cell)
              }
          )

          calleesGraph.varToCell.getOrElse(callSite.call, Map.empty).foreach{
            case (variable: Variable, oldSlice: Slice) =>
              val slice = callersGraph.find(oldSlice)
              val returnCells = calleesGraph.getCells(end(callee), variable)
              returnCells.foldLeft(calleesGraph.adjust(slice)){
                case (c: DSC, retCell: Slice) =>
                  calleesGraph.mergeCells(c, calleesGraph.adjust(retCell))
              }
            case _ => ???
          }
      )
      callersGraph.collectNodes
    td.toMap

  }
}
