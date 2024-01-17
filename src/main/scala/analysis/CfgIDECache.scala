package analysis

import scala.collection.mutable

class CfgIDECache {

  val entryExitMap: BiMap[CfgFunctionEntryNode, CfgProcedureReturnNode] = new BiMap[CfgFunctionEntryNode, CfgProcedureReturnNode]
  val callReturnMap: BiMap[CfgJumpNode, CfgCallReturnNode] = new BiMap[CfgJumpNode, CfgCallReturnNode]
  val callees: BiMap[CfgJumpNode, CfgFunctionEntryNode] = new BiMap[CfgJumpNode, CfgFunctionEntryNode]
  val afterCall: mutable.Map[CfgProcedureReturnNode, Set[CfgCallReturnNode]] = mutable.Map[CfgProcedureReturnNode, Set[CfgCallReturnNode]]()
  private var traversed: Set[CfgNode] = Set()

  def cacheCfg(cfg: ProgramCfg) = {

    cfg.funEntries.foreach(entry => {
      traversed = Set()
      traverse(entry, entry)
    })

    callReturnMap.domain.foreach(call => call.succInter.foreach{
      case s: CfgFunctionEntryNode =>
        callees.addOne(call, s)
      case _ =>
    })

    entryExitMap.backwardMap.foreach{
      case (exit, entry) =>
        val oldAfterCalls = afterCall.getOrElse(exit, Set())
        try {
          afterCall.put(exit, oldAfterCalls + callReturnMap(callees(entry)))
        } catch
          case _: NoSuchElementException =>
          case e => throw e
    }

//    entryExitMap.codomain.foreach(exit => exit.succInter.foreach {
//      case s: CfgCallReturnNode =>
//        val oldAfterCalls = afterCall.getOrElse(exit, Set())
//        afterCall.put(exit, oldAfterCalls + s)
//      case _ =>
//    })

  }

  private def traverse(entry: CfgFunctionEntryNode, cfgNode: CfgNode): Unit = {
    if (traversed.contains(cfgNode)) {
      return
    } else {
      traversed = traversed + cfgNode
    }

    cfgNode match
      case _: CfgFunctionExitNode =>
      case exit: CfgProcedureReturnNode =>
        entryExitMap.addOne((entry, exit))
      case call: CfgJumpNode =>
        call.succIntra.foreach {
          case ret: CfgCallReturnNode =>
            callReturnMap.addOne((call, ret))
            traverse(entry, ret)
          case node => traverse(entry, node)
        }
      case node =>
        node.succIntra.foreach{child => traverse(entry, child)}
  }
}



