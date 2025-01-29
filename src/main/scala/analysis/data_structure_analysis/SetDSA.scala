package analysis.data_structure_analysis

import analysis.data_structure_analysis.DSAPhase.Local
import analysis.solvers.{DSAUnionFindSolver, OffsetUnionFindSolver}
import cfg_visualiser.{DotStruct, DotStructElement, StructArrow, StructDotGraph}
import ir.Procedure
import util.DSALogger

import scala.collection.mutable.ArrayBuffer
import scala.collection.{SortedSet, mutable}

object SetNodeCounter extends Counter
class SetDSA


class SetGraph(proc: Procedure, phase: DSAPhase) extends SadGraph(proc, phase) {

  override def mergeCells(c1: SadCell, c2: SadCell): SadCell = {
    val cell1 = find(c1)
    val cell2 = find(c2)

    if cell1.equals(cell2) then
      cell1
    else if cell1.node.equals(cell2.node) then
      cell1.node.merge(cell1, cell2)
    else if cell1.node.isCollapsed || cell2.node.isCollapsed then
      collapseAndMerge(cell1, cell2)
    else
      mergeCellsHelper(cell1, cell2)

  }
}

object SetDSA {
  def getLocal(proc: Procedure): SetGraph = {
    val graph = SetGraph(proc, Local)
    graph.localPhase()
    graph
  }
}




