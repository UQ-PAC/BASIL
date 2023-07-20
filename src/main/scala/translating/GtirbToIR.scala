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
import com.grammatech.gtirb.proto.CFG.Edge._

/**
  * Currently, this does procedurers first by going through the function blocks and functionEntries maps. 
  * Hopefully this works, although more investigation will have to be done
  *
  */
class GtirbToIR(entrypoint: ByteString, functionEntries: mutable.Map[ByteString, mutable.Set[ByteString]], 
                functionBlocks: mutable.Map[ByteString, mutable.Set[ByteString]], cfg: CFG, parser: BilAdtParser) {

    def createIR() : Program = {

        var procedures : ArrayBuffer[Procedure] = ArrayBuffer()
        for (funcs <- 0 until functionEntries.size) {
            procedures(funcs) = createProcedure(functionEntries.keySet.toSeq(funcs))                                    
        }
    
        procedures = createJumps(procedures)

        val memorySections: ArrayBuffer[MemorySection] = ArrayBuffer()
        return Program(procedures, memorySections)
    }

    def createProcedure(uuid: ByteString) : Procedure = {
        val name = uuid.toString(); 
        val address : Option[Int] = None; //  TODO: - find where addresses are located
        val blocks : ArrayBuffer[Block] = createBlocks(uuid)
        val in : ArrayBuffer[Parameter] = ArrayBuffer() // TODO: gtirb does not contain this 
        val out : ArrayBuffer[Parameter] = ArrayBuffer() // TODO: gtirb does not contain this either 
        return Procedure(name, address, blocks, in, out)
    } 

    def getKey[K,V](value: V, map: mutable.Map[K,mutable.Set[V]]) : Option[K] = {
        val v = map.values.find(_.contains(value))
        val key = v.flatMap(v => map.find(_._2 == v).map(_._1))
        return key
    }

    def createBlocks(uuid: ByteString) : ArrayBuffer[Block] = {
        var blks : ArrayBuffer[Block] = ArrayBuffer[Block]()
        for (blk <- functionBlocks.get(uuid).get) {
            blks.addOne(createBlock(blk))
        }
        return blks
    }

    def createBlock(uuid: ByteString) : Block = {
        val address : Option[Int] = None //TODO: find where addresses are located
        val semantics : ArrayBuffer[Statement] = createSemantics(uuid) 
        val jumps : ArrayBuffer[Jump] = ArrayBuffer[Jump]()
        return Block(uuid.toString(), address, semantics, jumps)
    }

    def createJumps(procedure: ArrayBuffer[Procedure]) : ArrayBuffer[Procedure] = {
        for (p <- procedure) {
            for ( b <- p.blocks) {
                val uuid = ByteString.copyFromUtf8(b.label)
                val edges = cfg.edges.filter(_.sourceUuid.equals(uuid))
                for (edge <- edges) {
                    val targetuuid = edge.targetUuid 
                    val sourceuuid = edge.sourceUuid
                    val condition = edge.label.head.conditional 

                    val targetkey = getKey(targetuuid, functionBlocks).getOrElse(None)
                    val sourcekey = getKey(sourceuuid, functionBlocks).getOrElse(None)
                    val newfunctionkey = getKey(targetuuid, functionEntries).getOrElse(None)

                    if (targetkey.equals(sourcekey) && targetkey != None && sourcekey != None) {
                        val blk = p.blocks.find(_.label.equals(targetkey.toString())).get
                        b.jumps.addOne(GoTo(blk, None)) 
                        // TODO: pretty sure there's no way to find the condition -> maybe have to search through semantics again?
                    } else if (newfunctionkey != None && sourcekey != None) {
                        val func = procedure.find(_.name.equals(newfunctionkey.toString())).get
                        b.jumps.addOne(DirectCall(func, None, None)) // TODO: check return target 
                    } else if (newfunctionkey == None && targetkey == None) {
                        IndirectCall(???, ???, ???) //This one is nearly impossible to do without looking through semantics
                    }

                }
            }
        }   
        return procedure
    }  

    def createSemantics(uuid: ByteString) : ArrayBuffer[Statement] = {
        val visitor = SemanticsVisitor(uuid, parser.semantics())
        return visitor.createStatements()
    }
    
}
 