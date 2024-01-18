//package analysis
//
//import analysis.*
//import ir.*
//import util.Logger
//import util.RunUtils.writeToFile
//
//import scala.collection.mutable
//import scala.collection.mutable.ListBuffer
//
///** Steensgaard-style pointer analysis. The analysis associates an [[StTerm]] with each variable declaration and
//  * expression node in the AST. It is implemented using [[tip.solvers.UnionFindSolver]].
//  */
//class TeaDSA(
//      cfg: ProgramCfg,
//      constantProp: Map[CfgNode, Map[RegisterVariableWrapper, Set[BitVecLiteral]]],
//      globals: Map[BigInt, String],
//      globalOffsets: Map[BigInt, BigInt],
//      subroutines: Map[BigInt, String]) extends Analysis[Any] {
//
//
//  private type PointsToGraph = Map[Object, Set[VariableWrapper | MemoryRegion]]
//  private val procedurePointsTo = mutable.Map[CfgFunctionEntryNode, PointsToGraph]()
//
//
//  /** @inheritdoc
//    */
//  def analyze(): Unit =
//    // generate the constraints by traversing the AST and solve them on-the-fly
//    cfg.funEntries.foreach(fun => {
//      val steensForProc = new IntraprocSteensgaardAnalysis(fun, constantProp, globals, globalOffsets, subroutines)
//      steensForProc.analyze()
//      procedurePointsTo(fun) = steensForProc.pointsTo()
//    })
//    pointsToToDot()
//
//
//  def pointsToToDot(): Unit = {
//    val dot = new StringBuilder()
//    dot.append("digraph {\n")
//    procedurePointsTo.foreach { case (fun, pointsTo) =>
//      dot.append(s"  subgraph cluster_${fun.id} {\n")
//      dot.append(s"    label = \"${fun.data.name}\"\n")
//      pointsTo.foreach { case (obj, pts) =>
//        dot.append(s"    \"${obj.toString}\" [label = \"${obj.toString}\"]\n")
//        pts.foreach { pt =>
//          dot.append(s"    \"${obj.toString}\" -> \"${pt.toString}\"\n")
//        }
//      }
//      dot.append("  }\n")
//    }
//    dot.append("}\n")
//    writeToFile(dot.toString(), "pointsTo.dot")
//  }
//
//
//  /**
//   * Inlines the points-to graph of a procedure into the points-to graph of the caller.
//   */
//  def bottomUpMerge(callSite: CfgFunctionEntryNode, calleeGraph: PointsToGraph, callerGraph: PointsToGraph): Unit = {
//    // must inline the variables defined in this block with the other graph
//  }
//
//
//    /**
//     * Inlines the points-to graph context from call site into the points-to graph of the callee function.
//     */
//    def topDown(caller: CfgFunctionEntryNode, calleeGraph: PointsToGraph, callerGraph: PointsToGraph): Unit = {
//      caller.data.blocks.foreach(block =>
//        block.jump match {
//          case dc: DirectCall => {
//            // must inline the variables defined in this block with the other graphqq
//          }
//        }
//      )
//    }
//}