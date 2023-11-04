package analysis

import bap.{BAPJump, BAPSubroutine}
import ir.*
import specification.ExternalFunction

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Map, Queue}
import scala.collection.parallel.CollectionConverters.*

class TailCallOptimization {
  private val knownNonReturningFunctions = List("exit", "_exit", "abort", "__stack_chk_fail", "__assert_fail", "longjump")
  def transform(procedures: ArrayBuffer[Procedure], externalFunctions: Set[ExternalFunction]): Unit = {


    val mapJumpsToBlocks: Map[String, ArrayBuffer[(Jump, Block)]] = Map()
    val mapBlocksToProcedure: Map[String, (Procedure, Integer)] = Map()
    // look into each procedure, and calculate the number of return statements in each block
    // and create maps between jumps and blocks, and blocks and procedures.
    // this also looks at endless loops, and removes unreachable code after endless blocks
    for (proc <- procedures) {

      for ((block, index) <- proc.blocks.zipWithIndex) {
        mapBlocksToProcedure.addOne(block.label, (proc, index))
        for (jump <- block.jumps) {

          jump match {

            case directCall: DirectCall =>
              if (directCall.returnTarget.isEmpty) {

                val ic = IndirectCall(Register("R30", IntType),  None, None)
                val newBlock = ir.Block("",None, ArrayBuffer(), ArrayBuffer(ic))
                directCall.returnTarget = Some(newBlock)
              }
            case _ =>
          }

        }
      }
    }

  }

}
