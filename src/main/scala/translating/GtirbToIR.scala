package translating
import com.google.protobuf.ByteString
import scala.collection.mutable
import com.grammatech.gtirb.proto
import com.grammatech.gtirb.proto.CFG.CFG
import BilParser.BilAdtParser
import ir._
import scala.collection.mutable.ArrayBuffer
import java.awt.Taskbar.State
import java.util.Base64

class GtirbToIR(entrypoint: ByteString, functionEntries: mutable.Map[ByteString, mutable.Set[ByteString]], 
                functionBlocks: mutable.Map[ByteString, mutable.Set[ByteString]], cfg: CFG, parser: BilAdtParser) {

    def createIR() : Program = {
        val procedures : ArrayBuffer[Procedure] = ArrayBuffer()
        val startFunc = getKey(entrypoint, functionEntries) //Assumes Basic Blocks are unique in functions

        for (funcs <- 0 until functionEntries.size) {
            procedures(funcs) = createProcedure()
        }

        val memorySections: ArrayBuffer[MemorySection] = ArrayBuffer()
        Program(procedures, memorySections)
    }

    def createProcedure() : Procedure = {
        val name = ""; 
        val address : Int = 2; //  TODO - find where addresses are located
        val blocks : ArrayBuffer[Block] = ArrayBuffer()
        blocks(0) = createBlock(entrypoint)
        val in : ArrayBuffer[Parameter] = ArrayBuffer() // TODO: gtirb does not contain this 
        val out : ArrayBuffer[Parameter] = ArrayBuffer() // TODO: gtirb does not contain this either 
        return Procedure(name, address, blocks, in, out)
    } 

    def getKey[K,V](value: V, map: mutable.Map[K,mutable.Set[V]]) : K = {
        val v = map.values.filter(_.contains(value))
        val key = map.filter(_._2 == v).map(_._1)
        return key.head
    }

    def createBlock(uuid: ByteString) : Block = {
        val address : Option[Int] = None //TODO - find where addresses are located
        val semantics : ArrayBuffer[Statement] = ArrayBuffer() // visit Semantics 
        val edges = cfg.edges.filter(_.sourceUuid.equals(uuid))
        val jumps : ArrayBuffer[Jump] = ArrayBuffer()

        for (edge <- 0 until edges.size) {
            jumps(edge) = createJump(edges(edge))
        }
        
        return Block(uuid.toString(), address, semantics, jumps)
    }

    def createJump(edge: com.grammatech.gtirb.proto.CFG.Edge) : Jump = {
        // Search through function blocks to see if goto or call ?
        return ???
    }  

    def createSemantics(uuid: ByteString) : ArrayBuffer[Statement] = {
        val visitor = SemanticsVisitor(uuid, parser.semantics())
        return visitor.createStatements()
    }
    
}
