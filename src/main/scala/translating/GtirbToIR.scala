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
import scala.collection.mutable.HashMap
import java.nio.charset.*
import scala.util.boundary, boundary.break


/** Currently, this does procedurers first by going through the function blocks and functionEntries maps. Hopefully this
  * works, although more investigation will have to be done
  */
class GtirbToIR (mods: Seq[com.grammatech.gtirb.proto.Module.Module], parser: SemanticsParser, cfg: CFG) {

  def getKey[K, V](value: V, map: mutable.Map[K, mutable.Set[V]]): Option[K] = {
    val v = map.values.find(_.contains(value))
    val key = v.flatMap(v => map.find(_._2 == v).map(_._1))
    return key
  }

  def create_addresses(): collection.mutable.HashMap[ByteString, Int] = {

    val blockAddresses: HashMap[ByteString, Int] = HashMap.empty

    for {
      mod <- mods
      section <- mod.sections
      byteInterval <- section.byteIntervals
      block <- byteInterval.blocks
      if (!block.getCode.uuid.isEmpty)
    } {
      blockAddresses += (block.getCode.uuid -> (byteInterval.address + block.offset).toInt)
    }

    blockAddresses

  } 

  def create_cfg_map(): collection.mutable.HashMap[ByteString, ArrayBuffer[ByteString]] = {

    val edges = cfg.edges 
    val edgeMap: HashMap[ByteString, ArrayBuffer[ByteString]] = HashMap.empty

    for (edge <- edges) {

      if (edgeMap.contains(edge.sourceUuid)) {
        edgeMap(edge.sourceUuid) += edge.targetUuid

      } else {
        edgeMap += (edge.sourceUuid -> ArrayBuffer(edge.targetUuid))

      }

    }
    edgeMap
  }

  def create_symMap(): HashMap[ByteString, mutable.Set[proto.Symbol.Symbol]] = {
    val symMap = HashMap[ByteString, mutable.Set[proto.Symbol.Symbol]]()

    for (sym <- symbols) {
      if (sym.optionalPayload.isReferentUuid) {
        val ruuid = sym.optionalPayload.referentUuid.get

        if (symMap.contains(ruuid)) {
          symMap(ruuid) += sym
        } else {
          symMap += (ruuid -> mutable.Set[proto.Symbol.Symbol](sym))
        }
          
      } 
    }
    symMap
  }


  def create_names(uuid: ByteString): String = {
    var name: String = uuid.toString() 

    if (functionNames.get(uuid) != None) { 
      name = symbols.find(functionNames(uuid) == _.uuid).get.name //This is quite ineffecient if we have a ton of functions
      

      val entryBlocks: mutable.Set[ByteString] = functionEntries.get(uuid).get



      val result = entryBlocks
        .flatMap(e => edgeMap.getOrElse(e, None).iterator)
        .flatMap(elem => mods.flatMap(_.proxies).find(_.uuid == elem))
        .flatMap(proxy => symMap.getOrElse(proxy.uuid, None))
        .map(p => p.name)

      if (result.nonEmpty) {
        return result.head //. head seems weird here but i guess it works
      }

      val syms: mutable.Set[proto.Symbol.Symbol] = entryBlocks.flatMap(entry => 
        symMap.getOrElse(entry,None))
      val names: mutable.Set[String] = syms.map(elem => elem.name)
      
      
      if (names.size > 1) {
        var nam = ""
        nam += names.mkString(", ")
        return nam
      }
      
    }

    name
  }


  //TODO: mods.head may not work here if multiple modules
  val functionNames = MapDecoder.decode_uuid(mods.head.auxData.get("functionNames").get.data)
  val functionEntries = MapDecoder.decode_set(mods.head.auxData.get("functionEntries").get.data)
  val functionBlocks = MapDecoder.decode_set(mods.head.auxData.get("functionBlocks").get.data)
  val entrypoint = mods.head.entryPoint
  val symbols = mods.flatMap(_.symbols)
  val symMap = create_symMap()
  val addresses = create_addresses()
  val edgeMap: HashMap[ByteString, ArrayBuffer[ByteString]] = create_cfg_map()
 

  def createIR(): Program = {

    var procedures: ArrayBuffer[Procedure] = ArrayBuffer()

    functionEntries.keys.foreach { func =>
      procedures += createProcedure(func)
    }

    procedures = createJumps(procedures)

    val initialMemory: ArrayBuffer[MemorySection] = ArrayBuffer() // this looks like its incomplete
    val readOnlyMemory: ArrayBuffer[MemorySection] = ArrayBuffer() //ditto

    val intialproc: Procedure = createProcedure(getKey(entrypoint, functionEntries).get) // TODO: Does this work? -> They use readelf so i should probably do that

    return Program(procedures, intialproc, initialMemory, readOnlyMemory)
  }

  def createProcedure(uuid: ByteString): Procedure = {

    val name = create_names(uuid)
    val blocks: ArrayBuffer[Block] = createBlocks(uuid)
    
    
    
    val address: Option[Int] = addresses.get(functionEntries(uuid).head); //TODO: ask about entrypoints in BAP (or maybe function blocks is better here?)


    val in: ArrayBuffer[Parameter] = ArrayBuffer() // TODO: gtirb does not contain this -> Datablocks or symbols are candidates
    val out: ArrayBuffer[Parameter] = ArrayBuffer() // ditto above

    return Procedure(name, address, blocks, in, out)
  }

 

  def createBlocks(uuid: ByteString): ArrayBuffer[Block] = {
    var blks: ArrayBuffer[Block] = ArrayBuffer[Block]()
    var funcblks = functionBlocks.getOrElse(uuid, Set.empty[ByteString])
    // TODO: check this, because some procedures may randomly not have blocks

    if (funcblks.nonEmpty) {

      funcblks.foreach(elem => blks += createBlock(elem))

    } 
    return blks
  }

  def createBlock(uuid: ByteString): Block = {
    
    val address: Option[Int] = addresses.get(uuid)
    val semantics: ArrayBuffer[Statement] = createSemantics(uuid)
    val jump: Jump = GoTo(ArrayBuffer[Block](), None) //TODO: placeholder for now
    return Block(Base64.getEncoder().encodeToString(uuid.toByteArray()), address, semantics, jump) 
  }


  def createSemantics(uuid: ByteString): ArrayBuffer[Statement] = {

    var visitor = new SemanticsLoader(uuid, parser.semantics())
    val statements = visitor.createStatements()
    parser.reset()
    return statements

  }

  def createJumps(procedures: ArrayBuffer[Procedure]): ArrayBuffer[Procedure] = {

    val cpy = procedures
    val entries = functionEntries.values.flatten.toList
    val blocks = functionBlocks.values.flatten.toList


    for (p <- procedures) {

      for (b <- p.blocks) {
        val uuid = ByteString.copyFrom(Base64.getDecoder().decode(b.label))

        if (edgeMap.contains(uuid)) {

          val targets = edgeMap(uuid)
          val target = targets(0) // this seems uber naive but i'll leave it for now -> Will probably be changed

          b.jump = target match {
            case t if entries.contains(t) =>
              val key = getKey(t, functionEntries).get
              val proc = cpy.find(_.name == create_names(key)).get
              DirectCall(proc, Option(b), Option(proc.name))
              //potentially remove last stmt of block here since most likely just __PC call
            case t if (blocks.contains(t) && !entries.contains(t)) =>
              GoTo(ArrayBuffer[Block](), None)
            
            case _ =>
              // This match statement seems wacky but i guess its oki
              val reg : Variable = b.statements.last match {
                case l : LocalAssign => l.rhs match {
                  case v: Variable => v 
                  case e: Extract => e.body.asInstanceOf[Variable]
                }
                case m : MemoryAssign => Register("TEST", BitVecType(1))
              }
              //potentially remove last stmt of block here since most likely just __PC call
              IndirectCall(reg, Option(b), None)
          }

        }
        
      }
        
    }
    return procedures

  }

}
