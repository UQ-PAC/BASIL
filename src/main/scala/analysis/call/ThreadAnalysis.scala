package analysis.call

import analysis.call.ThreadAnalysis.threadProcedures
import analysis.{AddressValue, CallAnalysisInterface, CfgJumpNode, CfgNode, ConstantPropagationLattice, FlatLattice, ProgramCfgFactory, Value}
import ir.{BitVecLiteral, DirectCall, Expr, Literal, Parameter, Program, Thread, Variable}
import cfg_visualiser.ThreadOutput

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer



class ThreadAnalysis extends CallAnalysisInterface {


  override def processNode(
      dc: DirectCall,
      threadInfo: ThreadOutput,
      cfgNode: CfgNode,
      constantPropagationResult: Map[CfgNode, Map[Variable, Any]],
      program: Program,
      subroutines: Map[BigInt, String]
  ): Unit = {

    if (dc.target.name.startsWith("pthread_create")) {

      val element = constantPropagationResult(cfgNode).filter(_._1.name == "R2").head._2.asInstanceOf[ConstantPropagationLattice.FlatElement.FlatEl]
      val procedureName = subroutines(element.el.asInstanceOf[BitVecLiteral].value)
      val procedure = program.procedures.filter(_.name == procedureName).head
      val thread = Thread(startingPoint = procedure)
      threadInfo.nodes.addOne(thread)
      threadProcedures.put(dc, thread)
    }
  }

  override def processCFGDirectCall(
                                     programCfgFactory: ProgramCfgFactory,
                                     node: CfgJumpNode,
                                     directCall: DirectCall
  ): Unit = {
    if (threadProcedures.contains(directCall)) {
      programCfgFactory.cfgForProcedure(threadProcedures(directCall).startingPoint)
    }
  }
}


object ThreadAnalysis {
  val threadProcedures:mutable.HashMap[DirectCall, Thread] = mutable.HashMap[DirectCall,Thread]()
}