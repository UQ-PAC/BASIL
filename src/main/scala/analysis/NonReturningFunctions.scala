package analysis

import bap.{BAPJump, BAPSubroutine}
import ir.{Block, DirectCall, GoTo, IndirectCall, Jump, Procedure, Statement}

import scala.collection.mutable.{Map, Queue}
import collection.parallel.CollectionConverters.seqIsParallelizable
import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.CollectionConverters.*

class NonReturningFunctions {

  def transform(procedures: ArrayBuffer[Procedure]): Unit = {
    val blocksToRemove: Queue[String] = Queue()
    val mapJumpsToBlocks: Map[String, ArrayBuffer[(Jump, Block)]] = Map()
    val mapBlocksToProcedure: Map[String, (Procedure, Integer)] = Map()

    def isEndlessLoop(proc: Procedure, goTo: GoTo, index: Integer): Boolean = {

      if (goTo.condition.isEmpty && mapBlocksToProcedure.contains(goTo.target.label) && mapBlocksToProcedure(goTo.target.label)._2 < index) {

        val (_, idx) = mapBlocksToProcedure(goTo.target.label)

        for (loopIndex <- idx.toInt to index.toInt) {
          val blockAtLoopIndex = proc.blocks(loopIndex)
          for (loopJump <- blockAtLoopIndex.jumps) {
            loopJump match {
              case loopGoTo: GoTo =>
                if (!mapBlocksToProcedure.contains(loopGoTo.target.label) || mapBlocksToProcedure(loopGoTo.target.label)._2 >= index)
                  return false
              case call: IndirectCall =>
                if (call.target.name == "R30") {
                  return false
                }
              case _ =>
            }
          }

        }
        return true
      }
      false
    }

    for (proc <- procedures) {
      var numberOfReturns = 0
      for ((block, index) <- proc.blocks.zipWithIndex) {
        mapBlocksToProcedure.addOne(block.label, (proc, index))
        for (jump <- block.jumps) {

          jump match {
            case call: IndirectCall =>
              if (call.target.name == "R30") {
                block.countOfReturnStatements += 1
                numberOfReturns += 1
              }
            case directCall: DirectCall =>
              mapJumpsToBlocks.put(directCall.target.name, mapJumpsToBlocks.getOrElse(directCall.target.name, ArrayBuffer()).addOne((directCall, block)))
            case goTo: GoTo =>
              mapJumpsToBlocks.put(goTo.target.label, mapJumpsToBlocks.getOrElse(goTo.target.label, ArrayBuffer()).addOne((goTo, block)))
              if (proc.blocks.length > index && isEndlessLoop(proc, goTo, index)) {
                blocksToRemove.enqueue(proc.blocks(index+1).label)
              }
            case _ =>
          }

        }
      }
    }
    var blocksDeleted = true;

    while (blocksDeleted) {
      blocksDeleted = false
      for (proc <- procedures) {

        if (proc.calculateReturnCount() == 0) {
          mapJumpsToBlocks.get(proc.name) match {
            case Some(v) => for (block <- v) {
              val (_, containingBlock) = block
              for (jump <- containingBlock.jumps) {
                jump match {
                  case directCall: DirectCall =>
                    directCall.returnTarget match {
                      case Some(t) =>
                        blocksToRemove.enqueue(t.label)
                      case _ =>
                    }
                    directCall.returnTarget = None
                  case _ =>
                }
              }
            }
            case _ =>
          }
        }
      }

      while (blocksToRemove.nonEmpty) {
        val label = blocksToRemove.dequeue()
        val (procedure, _) = mapBlocksToProcedure(label)
        if (!mapJumpsToBlocks.contains(label) || mapJumpsToBlocks(label).length <= 1) {

          var procedureBlock: Integer = null
          for ((block, index) <- procedure.blocks.zipWithIndex) {
            if (block.label == label) {
              procedureBlock = index
              for (jump <- block.jumps) {
                jump match {
                  case goTo: GoTo =>
                    blocksToRemove.enqueue(goTo.target.label)
                  case _ =>
                }
              }
            }
          }
          if (procedureBlock != null) {
            procedure.blocks.remove(procedureBlock)
            blocksDeleted = true
          }
        }
      }
    }

  }
}
