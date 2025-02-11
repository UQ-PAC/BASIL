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


class SetGraph(proc: Procedure, phase: DSAPhase,
               symValues: Option[SymbolicValues] = None,
               cons: Option[Set[Constraint]] = None)
  extends SadGraph(proc, phase, symValues, cons) {

  override def mergeCells(c1: SadCell, c2: SadCell): SadCell = {
    val cell1 = find(c1)
    val cell2 = find(c2)

    val result = if cell1.equals(cell2) then
      cell1
    else if cell1.node.equals(cell2.node) then
      cell1.node.merge(cell1, cell2)
    else if cell1.node.isCollapsed || cell2.node.isCollapsed then
      collapseAndMerge(cell1, cell2)
    else
      mergeCellsHelper(cell1, cell2)

    assert(result == find(cell1))
    assert(result == find(cell2))
    if cell1.hasPointee then
      assert(result.getPointee == cell1.getPointee)
    if cell2.hasPointee then
      assert(result.getPointee == cell2.getPointee)
    result

  }

  override def collect(): (Set[SadNode], Set[(SadCell, SadCell)]) = {
    (Set.empty, Set.empty)
  }
}

object SetDSA {

  def getLocal(proc: Procedure, symValues: Option[SymbolicValues] = None,
               cons: Option[Set[Constraint]] = None,
              ): SetGraph = {
    val graph = SetGraph(proc, Local, symValues, cons)
    graph.localPhase()
    graph
  }
}




