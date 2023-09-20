package analysis

import ir.{DirectCall, Expr, Program, Variable}
import analysis.call.ThreadAnalysis
import analysis.{AddressValue, CallAnalysisInterface, CfgNode, Value}
import cfg_visualiser.ThreadOutput

abstract class CallAnalysisInterface {
  def processNode(
                   dc: DirectCall,
                   threadInfo: ThreadOutput,
                   cfgNode: CfgNode,
                   constantPropagationResult: Map[CfgNode, Map[Variable, Any]],
                   program: Program,
                   subroutines: Map[BigInt, String]
                 ): Unit

}
class DirectCallAnalysis {
  val threadAnalysis = ThreadAnalysis()
  val functionMap: Map[String,  CallAnalysisInterface] = Map("pthread_create" -> threadAnalysis)


  def processCall(
                   call: DirectCall,
                   threadInfo: ThreadOutput,
                   cfgNode: CfgNode,
                   constantPropagationResult: Map[CfgNode, Map[Variable, Any]],
                   program: Program,
                   subroutines: Map[BigInt, String]): Unit = {
      if (functionMap.contains(call.target.name))
        functionMap(call.target.name).processNode(call, threadInfo, cfgNode, constantPropagationResult, program, subroutines);
  }
}
