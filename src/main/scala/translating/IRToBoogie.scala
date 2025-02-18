package translating
import analysis.{RegionInjector, DataRegion, HeapRegion, MergedRegion}
import ir.{BoolOR, *}
import boogie.*
import specification.*
import util.{BoogieGeneratorConfig, BoogieMemoryAccessMode, ProcRelyVersion, Logger}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class IRToBoogie(
  var program: Program,
  var spec: Specification,
  var thread: Option[ProgramThread],
  val filename: String,
  val regionInjector: Option[RegionInjector],
  val config: BoogieGeneratorConfig
) {
  private val externAttr = BAttribute("extern")
  private val inlineAttr = BAttribute("inline")
  private val globals = spec.globals
  private val controls = spec.controls
  private val controlled = spec.controlled
  private val resolveSpec = ResolveSpec(regionInjector)
  private val resolveSpecL = ResolveSpecL(regionInjector)
  private val resolveOld = ResolveOld(resolveSpec)
  private val removeOld = RemoveOld(resolveSpec)
  private val relies = spec.relies.map(resolveSpec.visitBExpr)
  private val reliesParam = spec.relies.map(ResolveSpecParam.visitBExpr)
  private val reliesReflexive = spec.relies.map(removeOld.visitBExpr)
  private val guarantees = spec.guarantees.map(g => resolveOld.visitBExpr(g) -> g.oldSpecGlobals).toMap
  private val guaranteeRegions = guarantees.keys.map(g => g -> g.globals).toMap
  private val guaranteesParam = spec.guarantees.map(ResolveSpecParam.visitBExpr)
  private val guaranteesReflexive = spec.guarantees.map(removeOld.visitBExpr)
  private val requires = spec.subroutines.map(s => s.name -> s.requires.map(resolveSpec.visitBExpr)).toMap
  private val requiresDirect = spec.subroutines.map(s => s.name -> s.requiresDirect).toMap
  private val ensures = spec.subroutines.map(s => s.name -> s.ensures.map(resolveSpec.visitBExpr)).toMap
  private val ensuresDirect = spec.subroutines.map(s => s.name -> s.ensuresDirect).toMap
  private val libRelies = spec.subroutines.map(s => s.name -> s.rely).toMap
  private val libGuarantees = spec.subroutines.map(s => s.name -> s.guarantee).toMap
  private val directFunctions = spec.directFunctions

  private val mem = BMapVar("mem", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Global)
  private val Gamma_mem = BMapVar("Gamma_mem", MapBType(BitVecBType(64), BoolBType), Scope.Global)

  private val LPreds = spec.LPreds.map((k, v) => k -> resolveSpecL.visitBExpr(v))
  private val LArgs = lArgs

  private val memoriesToGamma = if (regionInjector.isDefined) {
    regionInjector.get
      .sharedRegions()
      .map { region =>
        val memory = BMapVar(region.name, MapBType(BitVecBType(64), BitVecBType(8)), Scope.Global)
        val gamma = BMapVar(s"Gamma_${region.name}", MapBType(BitVecBType(64), BoolBType), Scope.Global)
        memory -> gamma
      }
      .toMap
  } else {
    Map(mem -> Gamma_mem)
  }

  private val memoriesAndGammas: Set[BVar] = memoriesToGamma.flatMap((k, v) => Set(k, v)).toSet

  private val mem_in = BMapVar("mem$in", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Parameter)
  private val Gamma_mem_in = BMapVar("Gamma_mem$in", MapBType(BitVecBType(64), BoolBType), Scope.Parameter)
  private val mem_out = BMapVar("mem$out", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Parameter)
  private val Gamma_mem_out = BMapVar("Gamma_mem$out", MapBType(BitVecBType(64), BoolBType), Scope.Parameter)

  private val mem_inv1 = BMapVar("mem$inv1", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Local)
  private val Gamma_mem_inv1 = BMapVar("Gamma_mem$inv1", MapBType(BitVecBType(64), BoolBType), Scope.Local)
  private val mem_inv2 = BMapVar("mem$inv2", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Local)
  private val Gamma_mem_inv2 = BMapVar("Gamma_mem$inv2", MapBType(BitVecBType(64), BoolBType), Scope.Local)

  private val modifiedCheck: Set[BVar] = (for (i <- 19 to 29) yield {
    Set(BVariable("R" + i, BitVecBType(64), Scope.Global), BVariable("Gamma_R" + i, BoolBType, Scope.Global))
  }).flatten.toSet ++ Set(
    BVariable("R31", BitVecBType(64), Scope.Global),
    BVariable("Gamma_R31", BoolBType, Scope.Global)
  )

  def lArgs: List[BMapVar] = {
    if (regionInjector.isDefined) {
      spec.LPreds.values
        .flatMap(_.specGlobals)
        .toSet
        .map { g =>
          regionInjector.get.getMergedRegion(g.address, g.size) match {
            case Some(region) => BMapVar(s"${region.name}", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Global)
            case None => mem
          }
        }
        .toList
        .sorted
    } else {
      List(mem)
    }
  }

  def translate: BProgram = {
    val readOnlySections = program.usedMemory.values.filter(_.readOnly)
    val readOnlyMemory = memoryToCondition(readOnlySections)
    val initialSections = program.usedMemory.values.filter(!_.readOnly)
    val initialMemory = memoryToCondition(initialSections)

    val procedures: ArrayBuffer[BProcedure] = thread match {
      case None =>
        program.procedures.map(f => translateProcedure(f, readOnlyMemory, initialMemory))
      case Some(t) =>
        val translatedProcedures: ArrayBuffer[BProcedure] = ArrayBuffer[BProcedure]()
        t.procedures.foreach(p => translatedProcedures.addOne(translateProcedure(p, readOnlyMemory, initialMemory)))
        translatedProcedures
    }

    val guaranteeReflexive = BProcedure(
      name = "guarantee_reflexive",
      modifies = memoriesAndGammas,
      body = guaranteesReflexive.map(g => BAssert(g)),
      attributes = List(externAttr)
    )

    val rgProcs = genRely(relies, readOnlyMemory) :+ guaranteeReflexive

    val rgLib = config.procedureRely match {
      case Some(ProcRelyVersion.Function) =>
        // if rely/guarantee lib exist, create genRelyInv, and genInv for every procedure where rely/guarantee lib exist
        if (libRelies.values.flatten.nonEmpty && libGuarantees.values.flatten.nonEmpty) {
          List(genRelyInv) ++ libGuarantees.flatMap((k, v) =>
            if v.nonEmpty then genInv(k) :+ genLibGuarantee(k) else Nil
          )
        } else {
          List()
        }
      case Some(ProcRelyVersion.IfCommandContradiction) => libRGFunsContradictionProof.values.flatten
      case None => Nil
    }

    val functionsUsed1 = procedures.flatMap(p => p.functionOps).toSet ++
      rgProcs.flatMap(p => p.functionOps).toSet ++
      rgLib.flatMap(p => p.functionOps).toSet ++
      directFunctions

    val functionsUsed2 = functionsUsed1.map(p => functionOpToDefinition(p))
    val functionsUsed3 = functionsUsed2.flatMap(p => p.functionOps).map(p => functionOpToDefinition(p))
    val functionsUsed4 = functionsUsed3.flatMap(p => p.functionOps).map(p => functionOpToDefinition(p))
    val functionsUsed5 = functionsUsed4.flatMap(p => p.functionOps).map(p => functionOpToDefinition(p))
    val functionsUsed = (functionsUsed2 ++ functionsUsed3 ++ functionsUsed4 ++ functionsUsed5).toList.sorted

    val globalVars = procedures.flatMap(_.globals) ++ rgProcs.flatMap(_.globals)
    val globalDecls = globalVars.map(b => BVarDecl(b, List(externAttr))).distinct.sorted.toList

    val globalConsts: List[BConstAxiomPair] = globals
      .map { g =>
        BConstAxiomPair(BVarDecl(g.toAddrVar, List(externAttr)), g.toAxiom)
      }
      .toList
      .sorted

    val declarations =
      globalDecls ++ globalConsts ++ functionsUsed ++ rgLib ++ pushUpModifiesFixedPoint(rgProcs ++ procedures)
    BProgram(declarations, filename)
  }

  def genRely(relies: List[BExpr], readOnlyMemory: List[BExpr]): List[BProcedure] = {
    val reliesUsed = if (relies.nonEmpty) {
      relies
    } else {
      // default case where no rely is given - rely on no external changes
      memoriesAndGammas.toList.sorted.map(m => BinaryBExpr(BVEQ, m, Old(m)))
    }
    val relyEnsures = if (relies.nonEmpty) {
      val i = BVariable("i", BitVecBType(64), Scope.Local)

      val memImpliesGamma = memoriesToGamma.keys.toList.sorted.map { memory =>
        val gamma = memoriesToGamma(memory)
        ForAll(
          List(i),
          BinaryBExpr(
            BoolIMPLIES,
            BinaryBExpr(BVEQ, MapAccess(memory, i), Old(MapAccess(memory, i))),
            BinaryBExpr(BVEQ, MapAccess(gamma, i), Old(MapAccess(gamma, i)))
          )
        )
      }
      memImpliesGamma ++ reliesUsed
    } else {
      reliesUsed
    }
    val relyProc = BProcedure(
      "rely",
      ensures = relyEnsures,
      freeEnsures = readOnlyMemory,
      modifies = memoriesAndGammas,
      attributes = List(externAttr)
    )
    val relyTransitive = BProcedure(
      "rely_transitive",
      ensures = reliesUsed,
      modifies = memoriesAndGammas,
      body = List(BProcedureCall("rely"), BProcedureCall("rely")),
      attributes = List(externAttr)
    )
    val relyReflexive =
      BProcedure("rely_reflexive", body = reliesReflexive.map(r => BAssert(r)), attributes = List(externAttr))
    List(relyProc, relyTransitive, relyReflexive)
  }

  def genRelyInv: BProcedure = {
    val reliesUsed = if (reliesParam.nonEmpty) {
      reliesParam
    } else {
      // default case where no rely is given - rely on no external changes
      List(BinaryBExpr(BVEQ, mem_out, mem_in), BinaryBExpr(BVEQ, Gamma_mem_out, Gamma_mem_in))
    }
    val relyEnsures = if (reliesParam.nonEmpty) {
      val i = BVariable("i", BitVecBType(64), Scope.Local)
      val rely2 = ForAll(
        List(i),
        BinaryBExpr(
          BoolIMPLIES,
          BinaryBExpr(BVEQ, MapAccess(mem_out, i), MapAccess(mem_in, i)),
          BinaryBExpr(BVEQ, MapAccess(Gamma_mem_out, i), MapAccess(Gamma_mem_in, i))
        )
      )
      List(rely2) ++ reliesUsed
    } else {
      reliesUsed
    }
    BProcedure(
      "rely$inv",
      List(mem_in, Gamma_mem_in),
      List(mem_out, Gamma_mem_out),
      relyEnsures,
      attributes = List(externAttr)
    )
  }

  def genInv(name: String): List[BProcedure] = {
    // reliesParam OR procGuaranteeParam

    val reliesUsed = if (reliesParam.nonEmpty) {
      reliesParam
    } else {
      // default case where no rely is given - rely on no external changes
      List(BinaryBExpr(BVEQ, mem_out, mem_in), BinaryBExpr(BVEQ, Gamma_mem_out, Gamma_mem_in))
    }
    val relyEnsures = if (reliesParam.nonEmpty) {
      val i = BVariable("i", BitVecBType(64), Scope.Local)
      val rely2 = ForAll(
        List(i),
        BinaryBExpr(
          BoolIMPLIES,
          BinaryBExpr(BVEQ, MapAccess(mem_out, i), MapAccess(mem_in, i)),
          BinaryBExpr(BVEQ, MapAccess(Gamma_mem_out, i), MapAccess(Gamma_mem_in, i))
        )
      )
      List(rely2) ++ reliesUsed
    } else {
      reliesUsed
    }
    val relyOneLine = if (relyEnsures.size > 1) {
      relyEnsures.tail.foldLeft(relyEnsures.head)((ands: BExpr, next: BExpr) => BinaryBExpr(BoolAND, ands, next))
    } else {
      relyEnsures.head
    }

    val guaranteeEnsures = libGuarantees(name).map(ResolveSpecParam.visitBExpr)
    val guaranteeOneLine = if (guaranteeEnsures.size > 1) {
      guaranteeEnsures.tail.foldLeft(guaranteeEnsures.head)((ands: BExpr, next: BExpr) =>
        BinaryBExpr(BoolAND, ands, next)
      )
    } else {
      guaranteeEnsures.head
    }

    val invEnsures = List(BinaryBExpr(BoolOR, relyOneLine, guaranteeOneLine))

    val invProc = BProcedure(
      name + "$inv",
      List(mem_in, Gamma_mem_in),
      List(mem_out, Gamma_mem_out),
      invEnsures,
      attributes = List(externAttr)
    )

    val invTransitive = BProcedure(
      name + "$inv_transitive",
      List(mem_in, Gamma_mem_in),
      List(mem_out, Gamma_mem_out),
      invEnsures,
      body = List(
        BProcedureCall(name + "$inv", List(mem_out, Gamma_mem_out), List(mem_in, Gamma_mem_in)),
        BProcedureCall(name + "$inv", List(mem_out, Gamma_mem_out), List(mem_out, Gamma_mem_out))
      ),
      attributes = List(externAttr)
    )

    List(invProc, invTransitive)
  }

  def genLibGuarantee(name: String): BProcedure = {
    // G_f
    val guaranteeLib = libGuarantees(name).map(ResolveSpecParam.visitBExpr)
    val guaranteeOneLine = if (guaranteeLib.size > 1) {
      guaranteeLib.tail.foldLeft(guaranteeLib.head)((ands: BExpr, next: BExpr) => BinaryBExpr(BoolAND, ands, next))
    } else {
      guaranteeLib.head
    }
    val guaranteeAssume = BAssume(guaranteeOneLine)

    // G_c is ensures clause
    BProcedure(
      name + "$guarantee",
      List(mem_in, Gamma_mem_in),
      List(mem_out, Gamma_mem_out),
      guaranteesParam,
      body = List(guaranteeAssume),
      attributes = List(externAttr)
    )
  }

  /** A predicate used to assert the value of all readonly memory. (Boogie does not like this it if it is too large due
    * to it being a single expression)
    *
    * E.g. val readOnlyMemoryFunction = readOnlyMemoryPredicate(memoryToCondition(program.readOnlyMemory), mem) val
    * readOnlyMemory = List(BFunctionCall(readOnlyMemoryFunction.name, List(mem), BoolBType))
    */
  private def readOnlyMemoryPredicate(readonly: List[BExpr], mem: BVar): BFunction = {
    BFunction(
      "readonly_memory",
      List(BParam("mem", mem.bType)),
      BParam(BoolBType),
      Some(readonly.reduce((a, b) => BinaryBExpr(BoolAND, a, b))),
      List(externAttr)
    )
  }

  def functionOpToDefinition(f: FunctionOp): BFunction = {
    f match {
      case b @ BoolToBV1Op(arg) => {
        val invar = BParam("arg", BoolBType)
        val outvar = BParam(BitVecBType(1))
        val body = IfThenElse(invar, BitVecBLiteral(1, 1), BitVecBLiteral(0, 1))
        BFunction(b.fnName, List(invar), outvar, Some(body), List(externAttr))
      }
      case b: BVFunctionOp => BFunction(b.name, b.in, b.out, None, List(externAttr, b.attribute))
      case m: MemoryLoadOp =>
        val memVar = BMapVar("memory", MapBType(BitVecBType(m.addressSize), BitVecBType(m.valueSize)), Scope.Parameter)
        val indexVar = BParam("index", BitVecBType(m.addressSize))
        val in = List(memVar, indexVar)
        val out = BParam(BitVecBType(m.bits))
        val accesses: Seq[MapAccess] = for (i <- 0 until m.accesses) yield {
          if (i == 0) {
            MapAccess(memVar, indexVar)
          } else {
            MapAccess(memVar, BinaryBExpr(BVADD, indexVar, BitVecBLiteral(i, m.addressSize)))
          }
        }
        val accessesEndian = m.endian match {
          case Endian.BigEndian => accesses.reverse
          case Endian.LittleEndian => accesses
        }

        val body: BExpr = accessesEndian.toList match {
          case h :: Nil => h
          case h :: tail =>
            tail.foldLeft(h) { (concat: BExpr, next: MapAccess) =>
              BinaryBExpr(BVCONCAT, next, concat)
            }
          case Nil => throw Exception(s"Zero byte access: $f")
        }

        BFunction(m.fnName, in, out, Some(body), List(externAttr))
      case g: GammaLoadOp =>
        val gammaMapVar = BMapVar("gammaMap", MapBType(BitVecBType(g.addressSize), BoolBType), Scope.Parameter)
        val indexVar = BParam("index", BitVecBType(g.addressSize))
        val in = List(gammaMapVar, indexVar)
        val out = BParam(BoolBType)
        val accesses: Seq[MapAccess] = for (i <- 0 until g.accesses) yield {
          if (i == 0) {
            MapAccess(gammaMapVar, indexVar)
          } else {
            MapAccess(gammaMapVar, BinaryBExpr(BVADD, indexVar, BitVecBLiteral(i, g.addressSize)))
          }
        }

        val body: BExpr = accesses.tail.foldLeft(accesses.head) { (and: BExpr, next: MapAccess) =>
          BinaryBExpr(BoolAND, next, and)
        }

        BFunction(g.fnName, in, out, Some(body), List(externAttr))
      case m: MemoryStoreOp =>
        val memType = MapBType(BitVecBType(m.addressSize), BitVecBType(m.valueSize))
        val memVar = BMapVar("memory", memType, Scope.Parameter)
        val indexVar = BParam("index", BitVecBType(m.addressSize))
        val valueVar = BParam("value", BitVecBType(m.bits))
        val in = List(memVar, indexVar, valueVar)
        val out = BParam(memType)
        val body: BExpr = config.memoryFunctionType match {
          case BoogieMemoryAccessMode.SuccessiveStoreSelect =>
            val indices: Seq[BExpr] = for (i <- 0 until m.accesses) yield {
              if (i == 0) {
                indexVar
              } else {
                BinaryBExpr(BVADD, indexVar, BitVecBLiteral(i, m.addressSize))
              }
            }
            val values: Seq[BExpr] = for (i <- 0 until m.accesses) yield {
              BVExtract((i + 1) * m.valueSize, i * m.valueSize, valueVar)
            }
            val valuesEndian = m.endian match {
              case Endian.BigEndian => values.reverse
              case Endian.LittleEndian => values
            }
            val indiceValues = for (i <- 0 until m.accesses) yield {
              (indices(i), valuesEndian(i))
            }

            indiceValues.tail.foldLeft(MapUpdate(memVar, indices.head, valuesEndian.head)) {
              (update: MapUpdate, next: (BExpr, BExpr)) => MapUpdate(update, next(0), next(1))
            }

          case BoogieMemoryAccessMode.LambdaStoreSelect =>
            if (m.accesses == 1) {
              MapUpdate(memVar, indexVar, valueVar)
            } else {
              val i = BVariable("i", BitVecBType(m.addressSize), Scope.Local)
              Lambda(
                List(i),
                IfThenElse(
                  BInBounds(indexVar, BitVecBLiteral(m.accesses, m.addressSize), m.endian, i),
                  BByteExtract(valueVar, BinaryBExpr(BVSUB, i, indexVar)),
                  MapAccess(memVar, i)
                )
              )
            }
        }

        BFunction(m.fnName, in, out, Some(body), List(externAttr))
      case g: GammaStoreOp =>
        val gammaMapType = MapBType(BitVecBType(g.addressSize), BoolBType)
        val gammaMapVar = BMapVar("gammaMap", gammaMapType, Scope.Parameter)
        val indexVar = BParam("index", BitVecBType(g.addressSize))
        val valueVar = BParam("value", BoolBType)
        val in = List(gammaMapVar, indexVar, valueVar)
        val out = BParam(gammaMapType)

        val body: BExpr = config.memoryFunctionType match {
          case BoogieMemoryAccessMode.LambdaStoreSelect =>
            if g.accesses == 1 then MapUpdate(gammaMapVar, indexVar, valueVar)
            else {
              val i = BVariable("i", BitVecBType(g.addressSize), Scope.Local)
              Lambda(
                List(i),
                IfThenElse(
                  BInBounds(indexVar, BitVecBLiteral(g.accesses, g.addressSize), Endian.LittleEndian, i),
                  valueVar,
                  MapAccess(gammaMapVar, i)
                )
              )
            }
          case BoogieMemoryAccessMode.SuccessiveStoreSelect =>
            val indices: Seq[BExpr] = for (i <- 0 until g.accesses) yield {
              if (i == 0) {
                indexVar
              } else {
                BinaryBExpr(BVADD, indexVar, BitVecBLiteral(i, g.addressSize))
              }
            }
            val values: Seq[BExpr] = for (i <- 0 until g.accesses) yield {
              valueVar
            }
            val indiceValues = for (i <- 0 until g.accesses) yield {
              (indices(i), values(i))
            }
            indiceValues.tail.foldLeft(MapUpdate(gammaMapVar, indices.head, values.head)) {
              (update: MapUpdate, next: (BExpr, BExpr)) => MapUpdate(update, next(0), next(1))
            }
        }

        BFunction(g.fnName, in, out, Some(body), List(externAttr))
      case l: LOp =>
        val indexVar: BVar = BParam("index", l.indexType)
        val body: BExpr = LPreds.keys.foldLeft(FalseBLiteral) { (ite: BExpr, next: SpecGlobal) =>
          val guard = next.arraySize match {
            case Some(size: Int) =>
              val initial: BExpr = BinaryBExpr(BoolEQ, indexVar, ArrayAccess(next, 0).toAddrVar)
              val indices = 1 until size
              indices.foldLeft(initial) { (or: BExpr, i: Int) =>
                BinaryBExpr(BoolOR, BinaryBExpr(BoolEQ, indexVar, ArrayAccess(next, i).toAddrVar), or)
              }
            case None =>
              BinaryBExpr(BoolEQ, indexVar, next.toAddrVar)
          }
          val LPred = LPreds(next)
          /*if (controlled.contains(next)) {
          FunctionCall(s"L_${next.name}", List(l.memory), BoolType)
        } else {
          LPreds(next)
        } */
          IfThenElse(guard, LPred, ite)
        }
        val params = if (regionInjector.isDefined) {
          (body.params - indexVar).toList.sorted
        } else {
          List(BParam("mem$in", mem.bType))
        }
        BFunction("L", params :+ indexVar, BParam(BoolBType), Some(body), List(externAttr))
      case b: ByteExtract =>
        val valueVar = BParam("value", BitVecBType(b.valueSize))
        val offsetVar = BParam("offset", BitVecBType(b.offsetSize))
        val in = List(valueVar, offsetVar)
        val out = BParam(BitVecBType(8))
        val shift = BinaryBExpr(BVMUL, offsetVar, BitVecBLiteral(8, b.offsetSize))
        val eshift = if (b.valueSize < b.offsetSize) {
          BVExtract(b.valueSize, 0, shift)
        } else if (b.valueSize == b.offsetSize) {
          shift
        } else {
          BVZeroExtend(b.valueSize - b.offsetSize, shift)
        }
        val body = BVExtract(8, 0, BinaryBExpr(BVLSHR, valueVar, eshift))
        BFunction(b.fnName, in, out, Some(body), List(inlineAttr))
      case b: InBounds =>
        val baseVar = BParam("base", BitVecBType(b.bits))
        val lenVar = BParam("len", BitVecBType(b.bits))
        val iVar = BParam("i", BitVecBType(b.bits))
        val in = List(baseVar, lenVar, iVar)
        val out = BParam(BoolBType)
        val begin = b.endian match {
          case Endian.LittleEndian => baseVar
          case Endian.BigEndian => BinaryBExpr(BVSUB, baseVar, lenVar)
        }
        val end = b.endian match {
          case Endian.LittleEndian => BinaryBExpr(BVADD, baseVar, lenVar)
          case Endian.BigEndian => baseVar
        }

        val above = BinaryBExpr(BVULE, begin, iVar)
        val below = BinaryBExpr(BVULT, iVar, end)
        val wrap = BinaryBExpr(BVULE, baseVar, end)
        val body = IfThenElse(wrap, BinaryBExpr(BoolAND, above, below), BinaryBExpr(BoolOR, above, below))
        BFunction(b.fnName, in, out, Some(body), List(inlineAttr))

      case u: BUninterpreted =>
        BFunction(u.name, u.in.map(BParam(_)), BParam(u.out), None)
    }
  }

  def pushUpModifiesFixedPoint(procedures: List[BProcedure]): List[BProcedure] = {
    var changed = true
    var proceduresUpdated = procedures
    while (changed) {
      changed = false
      val nameToProcedure = proceduresUpdated.map(p => p.name -> p).toMap
      proceduresUpdated = proceduresUpdated.map { procedure =>
        val cmds: List[BCmd] = procedure.body.flatten {
          case b: BBlock => b.body
          case c: BCmd => Seq(c)
        }
        val callModifies = cmds.collect { case c: BProcedureCall => nameToProcedure(c.name) }.flatMap(_.modifies)
        val modifiesUpdate = procedure.modifies ++ callModifies
        if (modifiesUpdate != procedure.modifies) {
          changed = true
        }

        procedure.copy(modifies = modifiesUpdate)
      }
    }
    proceduresUpdated
  }

  def translateProcedure(p: Procedure, readOnlyMemory: List[BExpr], initialMemory: List[BExpr]): BProcedure = {
    val body = (p.entryBlock.view ++
      ArrayBuffer.from(p.blocks).sortBy(x => -x.rpoOrder).filterNot(x => p.entryBlock.contains(x)))
      .map(translateBlock)
      .toList

    val modifies: Seq[BVar] = p.modifies.toSeq
      .flatMap {
        case m: Memory => Seq(m.toBoogie, m.toGamma)
        case r: Register => Seq(r.toBoogie, r.toGamma)
      }
      .distinct
      .sorted

    val modifiedPreserve = modifies.collect { case m: BVar if modifiedCheck.contains(m) => m }
    val modifiedPreserveEnsures: List[BExpr] = modifiedPreserve.map(m => BinaryBExpr(BoolEQ, m, Old(m))).toList

    val procRequires: List[BExpr] = p.requires ++ requires.getOrElse(p.procName, List())
    val procEnsures: List[BExpr] = p.ensures ++ ensures.getOrElse(p.procName, List())

    val procRequiresDirect: List[String] = requiresDirect.getOrElse(p.procName, List())
    val procEnsuresDirect: List[String] = ensuresDirect.getOrElse(p.procName, List())

    val freeRequires: List[BExpr] = if (p == program.mainProcedure) {
      initialMemory ++ readOnlyMemory
    } else {
      readOnlyMemory
    }

    val freeEnsures = modifiedPreserveEnsures ++ readOnlyMemory

    val inparams = p.formalInParam.toList.flatMap(para => Seq(para.toBoogie, para.toGamma))
    val outparams = p.formalOutParam.toList.flatMap(para => Seq(para.toBoogie, para.toGamma))

    BProcedure(
      p.name,
      inparams,
      outparams,
      procEnsures,
      procRequires,
      procEnsuresDirect,
      procRequiresDirect,
      freeEnsures,
      freeRequires,
      modifies.toSet,
      body
    )
  }

  private def memoryToCondition(memorySections: Iterable[MemorySection]): List[BExpr] = {

    def coalesced: List[BExpr] = {
      val sections = memorySections
        .filter(_.size > 0)
        .flatMap { s =>
          // Phrase the memory condition in terms of 64-bit operations, as long as the memory
          // section's size is a multiple of 64-bits and 64-bits (8 bytes) aligned
          // If the memory section is not aligned, the initial unaligned part of it will not be coalesced into a 64-bit
          // representations and remain as an 8-bit representations
          // If the memory section's size is not a multiple of 64-bits, the last part of it that cannot be coalesced into
          // a 64-bit representation will remain as an 8-bit representation

          val memory = s.region match {
            case Some(region) => BMapVar(region.name, MapBType(BitVecBType(64), BitVecBType(8)), Scope.Global)
            case None => mem
          }

          if (s.bytes.size <= 8) {
            // if section is less than 8 bytes, just represent it with one access
            val combined = s.bytes.indices.foldLeft(BigInt(0))((x, y) => x + (s.bytes(y).value * BigInt(2).pow(y * 8)))
            val bits = s.bytes.size * 8
            Seq(
              BinaryBExpr(
                BVEQ,
                BMemoryLoad(memory, BitVecBLiteral(s.address, 64), Endian.LittleEndian, bits),
                BitVecBLiteral(combined, bits)
              )
            )
          } else {
            val aligned: Int = (s.address % 8).toInt

            val alignedSizeMultiple = (s.bytes.size - aligned) % 8
            // index of the byte that marks the end of the part that is a multiple of 64-bits
            val alignedEnd = s.bytes.size - alignedSizeMultiple

            // Aligned section that is safe to convert to 64-bit values
            val alignedSection = for (b <- aligned until alignedEnd by 8) yield {
              // Combine the byte constants into a 64-bit value
              val combined: BigInt =
                (0 until 8).foldLeft(BigInt(0))((x, y) => x + (s.bytes(b + y).value * BigInt(2).pow(y * 8)))
              BinaryBExpr(
                BVEQ,
                BMemoryLoad(memory, BitVecBLiteral(s.address + b, 64), Endian.LittleEndian, 64),
                BitVecBLiteral(combined, 64)
              )
            }

            // If memory section is somehow not aligned (is this possible?) then don't convert the initial non-aligned
            // section to 64-bit operations, just the rest
            val unalignedStartSection = if (aligned == 0) {
              Seq()
            } else {
              for (b <- 0 until aligned) yield {
                BinaryBExpr(
                  BVEQ,
                  BMemoryLoad(memory, BitVecBLiteral(s.address + b, 64), Endian.LittleEndian, 8),
                  s.bytes(b).toBoogie
                )
              }
            }

            // If the memory section is not a multiple of 64-bits then don't convert the last section to 64-bits
            // This is not ideal but will do for now
            // Ideal solution is to match the sizes based on the sizes listed in the symbol table, dividing further
            // for values greater than 64-bit as much as possible
            // But that requires more work
            // Combine the byte constants into a 64-bit value
            val unalignedEndSection = if (alignedSizeMultiple == 0) {
              Seq()
            } else {
              for (b <- alignedEnd until s.bytes.size) yield {
                BinaryBExpr(
                  BVEQ,
                  BMemoryLoad(memory, BitVecBLiteral(s.address + b, 64), Endian.LittleEndian, 8),
                  s.bytes(b).toBoogie
                )
              }
            }
            unalignedStartSection ++ alignedSection ++ unalignedEndSection
          }
        }
      sections.toList
    }

    def bytes: List[BExpr] = {
      val sections = memorySections.flatMap { s =>
        val memory = s.region match {
          case Some(region) => BMapVar(region.name, MapBType(BitVecBType(64), BitVecBType(8)), Scope.Global)
          case None => mem
        }
        for (b <- s.bytes.indices) yield {
          BinaryBExpr(
            BVEQ,
            BMemoryLoad(memory, BitVecBLiteral(s.address + b, 64), Endian.LittleEndian, 8),
            s.bytes(b).toBoogie
          )
        }
      }
      sections.toList
    }

    if config.coalesceConstantMemory then coalesced else bytes
  }

  def captureStateStatement(stateName: String): BAssume = {
    BAssume(TrueBLiteral, None, List(BAttribute("captureState", Some(s"\"$stateName\""))))
  }

  def translateBlock(b: Block): BBlock = {
    val captureState = captureStateStatement(s"${b.label}")
    val cmds = List(captureState) ++ b.statements.flatMap(s => translate(s)) ++ translate(b.jump)

    BBlock(b.label, cmds)
  }

  private val libRGFunsContradictionProof: Map[String, Seq[BProcedure]] = {

    /** Generate proof obligations for the library procedure rely/guarantee check.
      *
      *   1. \forall v' . Rc \/ Rv => (\forall v'' . (Rc => Rf) [(v, v')\ (v', v")) 2. Rc \/ Rv transitive 3. Gf => Gc
      *
      * (1.) is checked by an inline if block which c
      *
      * if (*) { assume (Rc \/ Rf) // v -> v' assume not(Rc => Rf) // v' -> v" assert false; }
      *
      * Procedures with no precond and the predicate as their postcond are generated to encode two-state assumptions.
      */
    (libRelies.keySet ++ libGuarantees.keySet)
      .filter(x => libRelies(x).nonEmpty && libGuarantees(x).nonEmpty)
      .map { targetName =>
        val Rc: BExpr = resolveSpec.visitBExpr(spec.relies.reduce((a, b) => BinaryBExpr(BoolAND, a, b)))
        val Gc: BExpr = resolveSpec.visitBExpr(spec.guarantees.reduce((a, b) => BinaryBExpr(BoolAND, a, b)))

        val Rf: BExpr = resolveSpec.visitBExpr(libRelies(targetName).reduce((a, b) => BinaryBExpr(BoolAND, a, b)))
        val Gf: BExpr = resolveSpec.visitBExpr(libGuarantees(targetName).reduce((a, b) => BinaryBExpr(BoolAND, a, b)))

        val inv = BinaryBExpr(BoolOR, Rc, Gf)
        val conseq = BinaryBExpr(BoolIMPLIES, Rc, Rf)
        val procInv = BProcedure(
          targetName + "$InlineInv",
          List(),
          List(),
          List(inv),
          List(),
          List(),
          List(),
          List(),
          List(),
          inv.globals,
          List(),
          List()
        )

        val proc2 = BProcedure(
          targetName + "$notRcimpliesRf",
          List(),
          List(),
          List(UnaryBExpr(BoolNOT, conseq)),
          List(),
          List(),
          List(),
          List(),
          List(),
          conseq.globals,
          List(),
          List()
        )

        val procGf = BProcedure(
          targetName + "$Gf",
          List(),
          List(),
          List(Gf),
          List(),
          List(),
          List(),
          List(),
          List(),
          Gf.globals,
          List(),
          List()
        )

        val proc4 = BProcedure(
          targetName + "$GfimpliesGc",
          List(),
          List(),
          List(Gc),
          List(),
          List(),
          List(),
          List(),
          List(),
          Gc.globals,
          List(BProcedureCall(procGf.name, List(), List()))
        )

        val proc5 = BProcedure(
          targetName + "$InlineInvTransitive",
          List(),
          List(),
          List(inv),
          List(),
          List(),
          List(),
          List(),
          List(),
          inv.globals,
          List(BProcedureCall(procInv.name, List(), List()), BProcedureCall(procInv.name, List(), List()))
        )

        targetName -> Seq(procInv, proc2, procGf, proc4, proc5)
      }
      .toMap
  }

  def relyfun(targetName: String): Option[IfCmd] = {
    libRGFunsContradictionProof.get(targetName).map { proc =>
      IfCmd(
        StarBLiteral,
        List(
          BProcedureCall(proc(0).name, Seq(), Seq()),
          BProcedureCall(proc(1).name, Seq(), Seq()),
          BAssert(FalseBLiteral)
        )
      )
    }
  }

  def translate(j: Jump): List[BCmd] = j match {
    case g: GoTo =>
      // collects all targets of the goto with a branch condition that we need to check the security level for
      // and collects the variables for that
      val conditions = g.targets.flatMap(_.statements.headOption.collect { case a: Assume if a.checkSecurity => a })
      val conditionVariables = conditions.flatMap(_.body.variables)
      val gammas = conditionVariables.map(_.toGamma).toList.sorted
      val conditionAssert: List[BCmd] = if (gammas.size > 1) {
        val andedConditions =
          gammas.tail.foldLeft(gammas.head)((ands: BExpr, next: BExpr) => BinaryBExpr(BoolAND, ands, next))
        List(BAssert(andedConditions))
      } else if (gammas.size == 1) {
        List(BAssert(gammas.head))
      } else {
        Nil
      }
      val jump = GoToCmd(g.targets.map(_.label).toSeq)
      conditionAssert :+ jump
    case r: Return => {
      val out = r.outParams.toList
      if (out.nonEmpty) then
        List(
          AssignCmd(out.map(_._1.toBoogie), out.map(_._2.toBoogie)),
          AssignCmd(out.map(_._1.toGamma), out.map(c => exprToGamma(c._2))),
          ReturnCmd
        )
      else List(ReturnCmd)
    }
    case _: Unreachable => List(BAssume(FalseBLiteral))
  }

  def translate(j: Call): List[BCmd] = j match {
    case d: DirectCall =>
      val call = BProcedureCall(
        d.target.name,
        d.outParams.toList.flatMap(c => Seq(c._2.toBoogie, c._2.gammas.head.toGamma)),
        d.actualParams.toList.flatMap(c => Seq(c._2.toBoogie, exprToGamma(c._2)))
      )

      (config.procedureRely match {
        case Some(ProcRelyVersion.Function) =>
          if (
            libRelies.contains(d.target.name) && libGuarantees
              .contains(d.target.name) && libRelies(d.target.name).nonEmpty && libGuarantees(d.target.name).nonEmpty
          ) {
            val invCall1 = BProcedureCall(d.target.name + "$inv", List(mem_inv1, Gamma_mem_inv1), List(mem, Gamma_mem))
            val invCall2 = BProcedureCall("rely$inv", List(mem_inv2, Gamma_mem_inv2), List(mem_inv1, Gamma_mem_inv1))
            val libRGAssert = libRelies(d.target.name).map(r => BAssert(ResolveSpecInv.visitBExpr(r)))
            List(invCall1, invCall2) ++ libRGAssert
          } else {
            List()
          }
        case Some(ProcRelyVersion.IfCommandContradiction) => relyfun(d.target.name).toList
        case None => List()
      }) ++ List(call)
    case i: IndirectCall => List(Comment(s"UNRESOLVED: call ${i.target.name}"), BAssert(FalseBLiteral))
  }

  def translate(s: Statement): List[BCmd] = s match {
    case d: Call => translate(d)
    case _: NOP => List.empty
    case m: MemoryStore =>
      val lhs = m.mem.toBoogie
      val rhs = BMemoryStore(m.mem.toBoogie, m.index.toBoogie, m.value.toBoogie, m.endian, m.size)
      val lhsGamma = m.mem.toGamma
      val rhsGamma = GammaStore(m.mem.toGamma, m.index.toBoogie, exprToGamma(m.value), m.size, m.size / m.mem.valueSize)
      val store = AssignCmd(List(lhs, lhsGamma), List(rhs, rhsGamma))
      val stateSplit = s match {
        case MemoryStore(_, _, _, _, _, Some(label)) => List(captureStateStatement(s"$label"))
        case LocalAssign(_, _, Some(label)) => List(captureStateStatement(s"$label"))
        case _ => List.empty
      }
      m.mem match {
        case _: StackMemory =>
          List(store) ++ stateSplit
        case _: SharedMemory =>
          val rely = BProcedureCall("rely")
          val gammaValueCheck = BAssert(BinaryBExpr(BoolIMPLIES, L(LArgs, rhs.index), exprToGamma(m.value)))
          val oldVars = guarantees.keys.view.toSet.flatMap { g =>
            if (guaranteeRegions(g).contains(lhs)) {
              guarantees(g)
            } else {
              Set()
            }
          }
          val oldAssigns = oldVars.toList.sorted.map { g =>
            val memory = if (regionInjector.isDefined) {
              regionInjector.get.getMergedRegion(g.address, g.size) match {
                case Some(region) => BMapVar(region.name, MapBType(BitVecBType(64), BitVecBType(8)), Scope.Global)
                case None => mem
              }
            } else {
              mem
            }
            AssignCmd(g.toOldVar, BMemoryLoad(memory, g.toAddrVar, Endian.LittleEndian, g.size))
          }
          val oldGammaAssigns = controlled.map { g =>
            val gamma = if (regionInjector.isDefined) {
              regionInjector.get.getMergedRegion(g.address, g.size) match {
                case Some(region) =>
                  BMapVar(s"Gamma_${region.name}", MapBType(BitVecBType(64), BoolBType), Scope.Global)
                case None =>
                  Gamma_mem
              }
            } else {
              Gamma_mem
            }
            AssignCmd(
              g.toOldGamma,
              BinaryBExpr(
                BoolOR,
                GammaLoad(gamma, g.toAddrVar, g.size, g.size / m.mem.valueSize),
                L(LArgs, g.toAddrVar)
              )
            )
          }
          val secureUpdate = for (c <- controls.keys.view.toSeq.sorted) yield {
            val addrCheck = BinaryBExpr(BVEQ, rhs.index, c.toAddrVar)
            val checks = controls(c).toList.sorted.map { v =>
              BinaryBExpr(BoolIMPLIES, L(LArgs, v.toAddrVar), v.toOldGamma)
            }
            val checksAnd = if (checks.size > 1) {
              checks.tail.foldLeft(checks.head)((next: BExpr, ands: BExpr) => BinaryBExpr(BoolAND, next, ands))
            } else {
              checks.head
            }
            BAssert(BinaryBExpr(BoolIMPLIES, addrCheck, checksAnd))
          }
          val guaranteeChecks = guarantees.keys.collect {
            case g if guaranteeRegions(g).contains(lhs) => BAssert(g)
          }
          (List(
            rely,
            gammaValueCheck
          ) ++ oldAssigns ++ oldGammaAssigns :+ store) ++ secureUpdate ++ guaranteeChecks ++ stateSplit
      }
    case l: LocalAssign =>
      val lhs = l.lhs.toBoogie
      val rhs = l.rhs.toBoogie
      val lhsGamma = l.lhs.toGamma
      val rhsGamma = exprToGamma(l.rhs)
      List(AssignCmd(List(lhs, lhsGamma), List(rhs, rhsGamma)))
    case m: MemoryLoad =>
      val lhs = m.lhs.toBoogie
      val lhsGamma = m.lhs.toGamma
      val rhs = BMemoryLoad(m.mem.toBoogie, m.index.toBoogie, m.endian, m.size)
      val rhsGamma = m.mem match {
        case s: StackMemory =>
          GammaLoad(s.toGamma, m.index.toBoogie, m.size, m.size / s.valueSize)
        case s: SharedMemory =>
          val boogieIndex = m.index.toBoogie
          BinaryBExpr(BoolOR, GammaLoad(s.toGamma, boogieIndex, m.size, m.size / s.valueSize), L(LArgs, boogieIndex))
      }
      val assign = AssignCmd(List(lhs, lhsGamma), List(rhs, rhsGamma))
      // add rely call if is a non-stack load
      m.mem match {
        case _: SharedMemory =>
          List(BProcedureCall("rely"), assign)
        case _ =>
          List(assign)
      }
    case a: Assert =>
      val body = a.body.toBoogie
      List(BAssert(body, a.comment))
    case a: Assume =>
      val body = a.body.toBoogie
      List(BAssume(body, a.comment))
  }

  def exprToGamma(e: Expr): BExpr = {
    val gammaVars: Set[BExpr] = e.gammas.map(_.toGamma)
    if (gammaVars.isEmpty) {
      TrueBLiteral
    } else if (gammaVars.size == 1) {
      gammaVars.head
    } else {
      gammaVars.tail.foldLeft(gammaVars.head) { (join: BExpr, next: BExpr) =>
        BinaryBExpr(BoolAND, next, join)
      }
    }
  }
}
