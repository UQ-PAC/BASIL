package analysis.call

import analysis.{AddressValue, CallAnalysisInterface, CfgNode, ConstantPropagationLattice, FlatLattice, Value}
import ir.{BitVecLiteral, DirectCall, Expr, Parameter, Program, Variable}
import cfg_visualiser.ThreadOutput

class ThreadAnalysis extends CallAnalysisInterface {
    override def processNode(
                              dc: DirectCall,
                              threadInfo: ThreadOutput,
                              cfgNode: CfgNode,
                              constantPropagationResult: Map[CfgNode, Map[Variable, Any]],
                              program: Program
                            ): Unit = {

      if (dc.target.name.startsWith("pthread_create")) {
        val resolvedValue = constantPropagationResult(cfgNode).filter(_._1.name == "R2")
        threadInfo.nodes.addOne(resolvedValue)
        println(threadInfo)
      }
    }
}
