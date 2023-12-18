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

  def get_jmp_reg(statement: Statement): Variable = statement match {
    case LocalAssign(_, rhs: Variable, _) => rhs
    case LocalAssign(_, rhs: Extract, _) => rhs.body.asInstanceOf[Variable]
    case _ => Register("TEST", BitVecType(1))
  }



  //TODO: mods.head may not work here if multiple modules
  val functionNames = MapDecoder.decode_uuid(mods.head.auxData.get("functionNames").get.data)
  val functionEntries = MapDecoder.decode_set(mods.head.auxData.get("functionEntries").get.data)
  val functionBlocks = MapDecoder.decode_set(mods.head.auxData.get("functionBlocks").get.data)
  val entrypoint = mods.head.entryPoint
  val symbols = mods.flatMap(_.symbols)
  val blkMap: HashMap[ByteString, Block] = HashMap[ByteString, Block]()
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
    val block = Block(Base64.getEncoder().encodeToString(uuid.toByteArray()), address, semantics, jump) 
    blkMap += (uuid -> block)
    return block
  }


  def createSemantics(uuid: ByteString): ArrayBuffer[Statement] = {

    var visitor = new SemanticsLoader(uuid, parser.semantics())
    val statements = visitor.createStatements()
    parser.reset()
    return statements

  }

  def singleJump(procedures: ArrayBuffer[Procedure], block: Block, target: ByteString, 
                  entries: List[ByteString], blocks: List[ByteString]): Jump = {
    target match {
      case t if entries.contains(t) => 
        val key = getKey(target, functionEntries).get
        val proc = procedures.find(_.name == create_names(key)).get
        return DirectCall(proc, None, Option(proc.name))
        //potentially remove last stmt of block here since most likely just __PC call

      case t if (blocks.contains(t) && !entries.contains(t)) => 
        return GoTo(ArrayBuffer[Block](blkMap(target)), None)

      case _ => 
        // This match statement seems wacky but i guess its oki
        val reg: Variable = get_jmp_reg(block.statements.last)
        return IndirectCall(reg, None, None)  
        //potentially remove last stmt of block here since most likely just __PC call     
    }
  }

  def multiJump(procedures: ArrayBuffer[Procedure], block: Block, targets: ArrayBuffer[ByteString], 
                entries: List[ByteString], blocks: List[ByteString]): Jump = {

    def resolveGotoTarget(): Block = {
    targets.find(!entries.contains(_)).map(blkMap).get
    }
    
    val hasDirectCall = targets.exists(entries.contains(_))
    val hasIndirectCall = !targets.exists(blocks.contains(_)) && !hasDirectCall
    val hasGoto = targets.exists(target => blocks.contains(target) && !entries.contains(target))

    
    (hasDirectCall, hasIndirectCall, hasGoto) match {
      case (true, _, true) =>
        val goto: Block = resolveGotoTarget()
        val target = targets.find(entries.contains).get 
        val key = getKey(target, functionEntries).get
        val proc = procedures.find(_.name == create_names(key)).get 
        DirectCall(proc, Option(goto), Option(proc.name))

      case (_, true, true) =>
        val goto: Block = resolveGotoTarget()
        val reg: Variable = get_jmp_reg(block.statements.last)
        IndirectCall(reg, Option(goto), None)

      case (_, _, true) =>
        val blks = targets.map(blkMap)
        GoTo(blks, None)

      case _ =>
        GoTo(ArrayBuffer[Block](), None)
    }

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

          if (targets.size > 1) {
            b.jump = multiJump(cpy, b, targets, entries, blocks)
          } else {
            val target = targets(0) // this seems uber naive but i'll leave it for now -> Will probably be changed
            b.jump = singleJump(cpy, b, target, entries, blocks)
          }
        }
   
      }    
    }

    return procedures
  }

}
