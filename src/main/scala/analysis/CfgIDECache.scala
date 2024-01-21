package analysis

import ir.{DirectCall, GoTo, IndirectCall}

import scala.collection.mutable

/**
 * Utility class for IDE solver
 */
class CfgIDECache {

  // Maps functions starts to function exit nodes
  val entryExitMap: BiMap[CfgFunctionEntryNode, CfgFunctionExitNode] = new BiMap[CfgFunctionEntryNode, CfgFunctionExitNode]

  // Maps Direct Jump nodes to Call Return Sites
  val callReturnMap: BiMap[CfgJumpNode, CfgCallReturnNode] = new BiMap[CfgJumpNode, CfgCallReturnNode]

  // Maps a direct call to a function entry node
  val callees: BiMap[CfgJumpNode, CfgFunctionEntryNode] = new BiMap[CfgJumpNode, CfgFunctionEntryNode]

  // Maps exit node to its successor set of Call return nodes
  val afterCall: mutable.Map[CfgFunctionExitNode, Set[CfgCallReturnNode]] = mutable.Map[CfgFunctionExitNode, Set[CfgCallReturnNode]]()

  private var traversed: Set[CfgNode] = Set()
  private var reversed: Set[CfgNode] = Set()

  /**
   * Reverses Cfg by swapping the direction of edges for backward Analyses using the IDE solver
   * Swaps places of Function entry/exit and call/return nodes
   * @param cfg Program Cfg
   */
  def reverseCfg(cfg: ProgramCfg) = {
    entryExitMap.forwardMap.foreach(
      (entry, exit) =>
        val tempIntra = entry.succIntra.clone()

        //TODO this won't work if entry has a branch where exit is reached immedietly and another where it is not
        if (!entry.succIntra.contains(exit))

          entry.succIntra.foreach(node =>
            node.predIntra -= entry
            node.predIntra += exit
          )

          exit.predIntra.foreach(node =>
            node.succIntra -= exit
            node.succIntra += entry
          )

          entry.succIntra --= entry.succIntra
          entry.succIntra ++= exit.predIntra

          exit.predIntra --= exit.predIntra
          exit.predIntra ++= tempIntra

          val entrySuccInter = entry.succInter.clone()
          val exitPredInter = exit.predInter.clone()

          entry.succInter.foreach(node =>
            node.predInter -= entry
            node.predInter += exit
          )

          exit.predInter.foreach(node =>
            node.succInter -= exit
            node.succInter += entry
          )

          entry.succInter --= entry.succInter
          entry.succInter ++= exit.predInter

          exit.predInter --= exit.predInter
          exit.predInter ++= entrySuccInter

        reversed = reversed ++ Vector(entry, exit)
    )

    cfg.nodes.foreach(node =>
      if (node.id == 196) {
        print("")
      }

      node match
        case jumpNode: CfgJumpNode => // makes indirect calls take intra procedural path in the analysis
          jumpNode.data match
            case call: IndirectCall =>
              jumpNode.succIntra.foreach {
                case ret: CfgCallReturnNode =>
                  jumpNode.succInter.add(ret)
                  ret.predInter.add(jumpNode)
                case noRet: CfgCallNoReturnNode =>
                  jumpNode.succInter.add(noRet)
                  noRet.predInter.add(jumpNode)
                case _ =>
              }
            case _ =>
        case _ =>
      if (!reversed.contains(node)) {
        swapEdges(node)
      }
    )

    callReturnMap.forwardMap.foreach(
      (call, ret) =>

        ret.predIntra.foreach( node =>
          node.succIntra -= ret
          node.succIntra += call
        )

        call.succIntra.foreach(node =>
          node.predIntra -= call
          node.predIntra += ret
        )

        ret.predInter.foreach(node =>
          node.succInter -= ret
          node.succInter += call
        )

        val temp = ret.predIntra.clone()
        ret.predIntra --= ret.predIntra
        ret.predIntra += call
        ret.succIntra --= ret.succIntra
        ret.succIntra ++= call.succIntra

        call.predIntra --= call.predIntra
        call.predIntra ++= temp
        call.succIntra --= call.succIntra
        call.succIntra += ret


        val callPred = call.predInter.clone()
        val callSucc = call.succInter.clone()


        call.predInter --= call.predInter
        call.predInter ++= ret.predInter
        call.succInter --= call.succInter
        call.succInter ++= callPred

        ret.predInter --= ret.predInter
        ret.predInter ++= ret.succInter
        ret.succInter --= ret.succInter
        ret.succInter ++= callSucc

    )
  }

  private def swapEdges(node: CfgNode): Unit = {
    var temp = node.succIntra.clone()
    node.succIntra --= node.succIntra
    node.succIntra ++= node.predIntra
    node.predIntra --= node.predIntra
    node.predIntra ++= temp

    temp = node.succInter.clone()
    node.succInter --= node.succInter
    node.succInter ++= node.predInter
    node.predInter --= node.predInter
    node.predInter ++= temp
  }

  /**
   * Caches Cfg Maps
   * Adds an edge from function exit to corresponding call return nodes
   * @param cfg Program Cfg
   */
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
          if (!exit.succInter.contains(callReturnMap(callees(entry)))) {
            cfg.addInterprocCallEdge(exit, callReturnMap(callees(entry)))
          }
        } catch
          case _: NoSuchElementException =>
          case e => throw e
    }
  }

  private def traverse(entry: CfgFunctionEntryNode, cfgNode: CfgNode): Unit = {
    if (traversed.contains(cfgNode)) {
      return
    } else {
      traversed = traversed + cfgNode
    }

    cfgNode match
      case exit: CfgFunctionExitNode =>
        entryExitMap.addOne((entry, exit))
      case call: CfgJumpNode if call.data.isInstanceOf[DirectCall] =>
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



