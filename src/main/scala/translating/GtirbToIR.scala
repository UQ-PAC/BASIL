package translating
import com.google.protobuf.ByteString
import scala.collection.mutable
import com.grammatech.gtirb.proto
import com.grammatech.gtirb.proto.CFG.CFG
import com.grammatech.gtirb.proto.IR.IR
import com.grammatech.gtirb.proto.Module.Module
import com.grammatech.gtirb.proto.Section.Section
import Parsers.*
import gtirb.*
import ir._
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters._
import java.awt.Taskbar.State
import java.util.Base64
import com.grammatech.gtirb.proto.CFG.Edge._

/** Currently, this does procedurers first by going through the function blocks and functionEntries maps. Hopefully this
  * works, although more investigation will have to be done
  */
class GtirbToIR (mods: Seq[com.grammatech.gtirb.proto.Module.Module], parser: SemanticsParser, cfg: CFG) {

  val functionNames = MapDecoder.decode_uuid(mods.head.auxData.get("functionNames").get.data)
  val functionEntries = MapDecoder.decode_set(mods.head.auxData.get("functionEntries").get.data)
  val functionBlocks = MapDecoder.decode_set(mods.head.auxData.get("functionBlocks").get.data)
  val entrypoint = mods.head.entryPoint
  val symbols = mods.flatMap(_.symbols)

  def createIR(): Program = {

    var procedures: ArrayBuffer[Procedure] = ArrayBuffer()

    functionEntries.keys.foreach { func =>
      procedures += createProcedure(func)
    }

    // procedures = createJumps(procedures)

    val initialMemory: ArrayBuffer[MemorySection] = ArrayBuffer() // this looks like its incomplete
    val readOnlyMemory: ArrayBuffer[MemorySection] = ArrayBuffer() //ditto

    val intialproc: Procedure = createProcedure(entrypoint) // TODO: Does this work?

    return Program(procedures, intialproc, initialMemory, readOnlyMemory)
  }

  def createProcedure(uuid: ByteString): Procedure = {

    var name = uuid.toString() 
    if (functionNames.get(uuid) != None){ 
      name = symbols.find(functionNames(uuid) == _.uuid).get.name
    }

    val address: Option[Int] = None; //  TODO: - find where addresses are located

    val blocks: ArrayBuffer[Block] = createBlocks(uuid)
    val in: ArrayBuffer[Parameter] = ArrayBuffer() // TODO: gtirb does not contain this
    val out: ArrayBuffer[Parameter] = ArrayBuffer() // TODO: gtirb does not contain this either
    return Procedure(name, address, blocks, in, out)
  }

  def getKey[K, V](value: V, map: mutable.Map[K, mutable.Set[V]]): Option[K] = {
    val v = map.values.find(_.contains(value))
    val key = v.flatMap(v => map.find(_._2 == v).map(_._1))
    return key
  }

  def createBlocks(uuid: ByteString): ArrayBuffer[Block] = {
    var blks: ArrayBuffer[Block] = ArrayBuffer[Block]()
    var funcblks = functionBlocks.getOrElse(uuid, Set.empty[ByteString])

    if (funcblks.nonEmpty) {

      funcblks.foreach(elem => blks += createBlock(elem))

    } else {
      // TODO: check this, in what case is a basic block not in the functionblocks?
    }

    return blks
  }

  def createBlock(uuid: ByteString): Block = {
    val address: Option[Int] = None //TODO: find where addresses are located

    val semantics: ArrayBuffer[Statement] = createSemantics(uuid)
    val jump: Jump = GoTo(ArrayBuffer[Block](), None) //TODO: placeholder for now
    return Block(uuid.toString(), address, semantics, jump) 
  }

  def createJumps(procedure: ArrayBuffer[Procedure]): ArrayBuffer[Procedure] = {
    for (p <- procedure) {

      for (b <- p.blocks) {
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
            val blockbuffer: ArrayBuffer[Block] = ArrayBuffer[Block]()
            blockbuffer.addOne(blk)

            b.jump = GoTo(blockbuffer, None) //TODO: Have no idea why suddenlu these take an array, ask about this
            // TODO: pretty sure there's no way to find the condition -> maybe have to search through semantics again?
          } else if (newfunctionkey != None && sourcekey != None) {
            val func = procedure.find(_.name.equals(newfunctionkey.toString())).get
            b.jump = DirectCall(func, None, None) // TODO: check return target
          } else if (newfunctionkey == None && targetkey == None) {
            IndirectCall(???, ???, ???) //This one is nearly impossible to do without looking through semantics
          }

        }
      }

    }
    return procedure
  }

  def createSemantics(uuid: ByteString): ArrayBuffer[Statement] = {

    var visitor = new SemanticsLoader(uuid, parser.semantics())
    val statements = visitor.createStatements()
    parser.reset()
    return statements

  }

}
