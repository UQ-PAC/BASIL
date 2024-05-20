package analysis

import ir.{BitVecLiteral, BitVecType, CFGPosition, CallGraph, Procedure, Program, Register, Variable, computeDomain, end}
import specification.{ExternalFunction, SpecGlobal}

import scala.collection.mutable

class DSA(program: Program,
            symResults: Map[CFGPosition, Map[SymbolicAccess, TwoElement]],
            constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
            globals: Set[SpecGlobal], globalOffsets: Map[BigInt, BigInt],
            externalFunctions: Set[ExternalFunction],
            reachingDefs: Map[CFGPosition, Map[Variable, Set[CFGPosition]]],
            writesTo: Map[Procedure, Set[Register]],
            params:  Map[Procedure, Set[Variable]]
         ) extends Analysis[Map[Procedure, DSG]] {

  val locals : mutable.Map[Procedure, DSG] = mutable.Map()
  val bu: mutable.Map[Procedure, DSG] = mutable.Map()
  val td: mutable.Map[Procedure, DSG] = mutable.Map()

  val stackPointer = Register("R31", BitVecType(64))
  val returnPointer = Register("R30", BitVecType(64))
  val framePointer = Register("R29", BitVecType(64))

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
    val domain = computeDomain(CallGraph, Set(program.mainProcedure))
    domain.foreach(
      proc =>
        val dsg = Local(proc, symResults, constProp, globals, globalOffsets, externalFunctions, reachingDefs, writesTo, params).analyze()
        locals.update(proc, dsg)
        bu.update(proc, dsg.cloneSelf())
    )

    val leafNodes = findLeaf(program.mainProcedure)

    leafNodes.foreach(
      proc =>
        assert(locals(proc).callsites.isEmpty)
        visited += proc
        queue.enqueueAll(CallGraph.pred(proc).diff(visited))
//        CallGraph.pred(proc).foreach(buildBUQueue)
    )

    while queue.nonEmpty do
      val proc = queue.dequeue()
      visited += proc
      queue.enqueueAll(CallGraph.pred(proc).diff(visited))
      val buGraph = bu(proc)
      buGraph.callsites.foreach( // clone all the nodes first
        callSite =>
          val callee = callSite.proc
          val calleeGraph = locals(callee).cloneSelf()
          assert(calleeGraph.formals.keySet.diff(ignoreRegisters).equals(callSite.paramCells.keySet))
          calleeGraph.formals.foreach{
            case (variable: Variable, (cell: DSC, internalOffset: BigInt)) if !ignoreRegisters.contains(variable)  =>
              assert(callSite.paramCells.contains(variable))
              val node = cell.node.get
              node.cloneNode(calleeGraph, buGraph)
            case _ =>
          }

          assert(writesTo(callee).equals(callSite.returnCells.keySet))
          writesTo(callee).foreach(
            reg =>
              assert(callSite.returnCells.contains(reg))
              val returnCells = calleeGraph.getCells(end(callee), reg)
              assert(returnCells.nonEmpty)
              returnCells.foreach{
                case (cell: DSC, internalOffset: BigInt) =>
                  val node = cell.node.get
                  node.cloneNode(calleeGraph, buGraph)
              }
          )
      )
      buGraph.callsites.foreach(//unify nodes
        callSite =>
          val callee = callSite.proc
          val calleeGraph = locals(callee).cloneSelf()
          calleeGraph.formals.foreach{
            case (variable: Variable, formal) if !ignoreRegisters.contains(variable)  =>
                buGraph.mergeCells(adjust(formal), adjust(callSite.paramCells(variable)))
            case _ =>
          }
          writesTo(callee).foreach(
            reg =>
              val returnCells = calleeGraph.getCells(end(callee), reg)
//              assert(returnCells.nonEmpty)
              returnCells.foldLeft(adjust(callSite.returnCells(reg))){
                case (c: DSC, ret) =>
                  buGraph.mergeCells(c, adjust(ret))
              }
          )
      )
    // bottom up phase finished
    // clone bu graphs to top-down graphs
    domain.foreach(
      proc =>
        td.update(proc, bu(proc).cloneSelf())
    )

    queue.enqueue(program.mainProcedure)
    visited = Set()
    while queue.nonEmpty do
      val proc = queue.dequeue()
      visited += proc
      queue.enqueueAll(CallGraph.succ(proc).diff(visited))
      val callersGraph = td(proc)
      callersGraph.callsites.foreach(
        callSite =>
          val callee = callSite.proc
          val calleesGraph = td(callee)
          callSite.paramCells.foreach{
            case (variable: Variable, (cell: DSC, internalOffset: BigInt)) =>
              val node = cell.node.get
              node.cloneNode(callersGraph, calleesGraph)
          }

          callSite.returnCells.foreach{
            case (variable: Variable, (cell: DSC, internalOffset: BigInt)) =>
              val node = cell.node.get
              node.cloneNode(callersGraph, callersGraph)
          }
      )

      callersGraph.callsites.foreach(
        callSite =>
          val callee = callSite.proc
          val calleesGraph = td(callee)
          callSite.paramCells.foreach {
            case (variable: Variable, cell) =>
              calleesGraph.mergeCells(adjust(cell), adjust(calleesGraph.formals(variable)))
          }

          callSite.returnCells.foreach {
            case (variable: Variable, cell: (DSC, BigInt)) =>
              val returnCells = calleesGraph.getCells(end(callee), variable)
              returnCells.foldLeft(adjust(cell)){
                case (c: DSC, retCell: (DSC, BigInt)) =>
                  calleesGraph.mergeCells(c, adjust(retCell))
              }
          }
      )

    td.toMap

  }
}
