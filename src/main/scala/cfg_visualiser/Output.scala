package cfg_visualiser

import java.io.{File, PrintWriter}
import analysis._

/** Basic outputting functionality.
  */
object Output {

  /** Helper function for producing string output for a control-flow graph node after an analysis.
    * @param res
    *   map from control-flow graph nodes to strings, as produced by the analysis
    */
  def labeler(res: Map[CfgNode, _], stateAfterNode: Boolean)(n: CfgNode): String = {
    val r = res.getOrElse(n, "-")
    val desc = n match {
      case entry: CfgFunctionEntryNode => s"Function ${entry.data.name} entry"
      case exit: CfgFunctionExitNode   => s"Function ${exit.data.name} exit"
      case _                           => n.toString
    }
    if (stateAfterNode) s"$desc\n$r"
    else s"$r\n$desc"
  }

  /** Generate an unique ID string for the given AST node.
    */
  def dotIder(n: CfgNode, uniqueId: Int): String =
    n match {
      case real: CfgCommandNode           => s"real${real.data}_$uniqueId"
      case entry: CfgFunctionEntryNode    => s"entry${entry.data}_$uniqueId"
      case exit: CfgFunctionExitNode      => s"exit${exit.data}_$uniqueId"
      case ret: CfgProcedureReturnNode    => s"return_$uniqueId"
      case noCallRet: CfgCallNoReturnNode => s"callnoreturn_$uniqueId"
      case callRet: CfgCallReturnNode     => s"callreturn_$uniqueId"
      case _                              => ???
    }
}