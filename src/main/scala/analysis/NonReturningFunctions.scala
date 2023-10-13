package analysis

import bap.{BAPJump, BAPSubroutine}
import ir.{Block, DirectCall, GoTo, IndirectCall, Jump, Procedure, Statement}
import specification.ExternalFunction

import scala.collection.mutable.{Map, Queue}
import collection.parallel.CollectionConverters.seqIsParallelizable
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.CollectionConverters.*

class NonReturningFunctions {
  private val knownNonReturningFunctions = List("exit", "_exit", "abort", "__stack_chk_fail", "__assert_fail", "longjump")
  def transform(procedures: ArrayBuffer[Procedure], externalFunctions: Set[ExternalFunction]): Unit = {
    val blocksToRemove: Queue[String] = Queue()
    val mapJumpsToBlocks: Map[String, ArrayBuffer[(Jump, Block)]] = Map()
    val mapBlocksToProcedure: Map[String, (Procedure, Integer)] = Map()

    val externalFunctionNames = externalFunctions.map(func => func.name)

    // look into each procedure, and calculate the number of return statements in each block
    // and create maps between jumps and blocks, and blocks and procedures.
    // this also looks at endless loops, and removes unreachable code after endless blocks
    for (proc <- procedures) {

      for ((block, index) <- proc.blocks.zipWithIndex) {
        mapBlocksToProcedure.addOne(block.label, (proc, index))
        for (jump <- block.jumps) {

          jump match {
            case call: IndirectCall =>
              if (call.target.name == "R30") {
                block.countOfReturnStatements += 1
              }
            case directCall: DirectCall =>

              if (knownNonReturningFunctions.contains(directCall.target.name)) {
                directCall.returnTarget = None
              }

              mapJumpsToBlocks.put(directCall.target.name, mapJumpsToBlocks.getOrElse(directCall.target.name, ArrayBuffer()).addOne((directCall, block)))

            case goTo: GoTo =>
              mapJumpsToBlocks.put(goTo.target.label, mapJumpsToBlocks.getOrElse(goTo.target.label, ArrayBuffer()).addOne((goTo, block)))

            case _ =>
          }

        }
      }
    }

    var blocksDeleted = true
    while (blocksDeleted) {
      blocksDeleted = false

      // find all direct calls that are non-returning, not an external function and currently have a return target.
      // add the return targets to the queue for removal, and mark the function as non-returning
      for (proc <- procedures) {
        if (!externalFunctionNames.contains(proc.name) && proc.calculateReturnCount() == 0) {
          mapJumpsToBlocks.get(proc.name) match {
            case Some(v) =>
              for (block <- v) {
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

      // For each block in the queue, attempt to remove the block if there are no links to the block from other parts of
      // the program, and add to queue any jumps that the removed blocks reference
      while (blocksToRemove.nonEmpty) {
        val label = blocksToRemove.dequeue()
        val (procedure, _) = mapBlocksToProcedure(label)
        if (!mapJumpsToBlocks.contains(label) || mapJumpsToBlocks(label).length <= 1) {

          var procedureBlock: Option[Integer] = None
          for ((block, index) <- procedure.blocks.filter(!_.deleted).zipWithIndex) {
            if (block.label == label) {
              procedureBlock = Some(index)
              for (jump <- block.jumps) {
                jump match {
                  case goTo: GoTo =>
                    blocksToRemove.enqueue(goTo.target.label)
                  case _ =>
                }
              }
            }
          }

          procedureBlock match {
            case Some(x) =>
              procedure.blocks(x).deleted = true
              blocksDeleted = true
            case _ =>
          }
        }
      }
    }

  }

  def trimDeletedBlocks(procedures: ArrayBuffer[Procedure]): Unit = {
    for (proc <- procedures) {
      proc.blocks = proc.blocks.filter(!_.deleted)
    }
  }
}
