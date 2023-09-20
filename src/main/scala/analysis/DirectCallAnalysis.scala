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
  def processCFGDirectCall(
                            programCfgFactory: ProgramCfgFactory,
                            node: CfgJumpNode,
                            directCall: DirectCall
                          ): Unit
}
object DirectCallAnalysis {
  val functionMap: Map[String,  CallAnalysisInterface] = Map("pthread_create" -> ThreadAnalysis());

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

  def processCFGDirectCall(programCfgFactory: ProgramCfgFactory, node: CfgJumpNode, directCall: DirectCall): Unit = {
    if (functionMap.contains(directCall.target.name))
      functionMap(directCall.target.name).processCFGDirectCall(programCfgFactory, node, directCall);
  }
}
