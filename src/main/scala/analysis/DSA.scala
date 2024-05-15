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
         ) extends Analysis[Any] {

  val locals : mutable.Map[Procedure, DSG] = mutable.Map()
  val bu: mutable.Map[Procedure, DSG] = mutable.Map()

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

  def getCells(pos: CFGPosition, arg: Variable, graph: DSG): Set[(DSC, BigInt)] =
    if reachingDefs(pos).contains(arg) then
      reachingDefs(pos)(arg).foldLeft(Set[(DSC, BigInt)]()) {
        (s, defintion) =>
          s + graph.varToCell(defintion)(arg)
      }
    else
      Set(graph.formals(arg))

  var visited = Set[Procedure]()
  val queue = mutable.Queue[Procedure]()
  override def analyze(): Any = {
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
        val preds = CallGraph.pred(proc)
        queue.enqueueAll(CallGraph.pred(proc).diff(visited))
    )

    while queue.nonEmpty do
      val proc = queue.dequeue()
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
            case (variable: Variable, (cell: DSC, internalOffset: BigInt)) if !ignoreRegisters.contains(variable)  =>
                buGraph.mergeCells(cell, callSite.paramCells(variable))
            case _ =>
          }
          writesTo(callee).foreach(
            reg =>
              val returnCells = calleeGraph.getCells(end(callee), reg)
//              assert(returnCells.nonEmpty)
              returnCells.foldLeft(callSite.returnCells(reg)){
                case (c: DSC, (cell: DSC, internalOffset: BigInt)) =>
                  buGraph.mergeCells(c, cell)
              }
          )

      )


    println(bu)
  }
}
