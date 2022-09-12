package translating

import astnodes._
import boogie._
import specification._

case class BoogieTranslator(program: Program, spec: Specification) {
  private val globals = spec.globals
  private val controls = spec.controls
  private val controlled = spec.controlled
  private val relies = spec.relies.map(r => r.resolveSpec)
  private val reliesReflexive = spec.relies.map(r => r.removeOld)
  private val guarantees = spec.guarantees.map(g => g.resolveOld)
  private val guaranteesReflexive = spec.guarantees.map(g => g.removeOld)
  private val guaranteeOldVars = spec.guaranteeOldVars
  private val LPreds = spec.LPreds.map((k, v) => k -> v.resolveSpecL)
  private val gammaInits = spec.gammaInits.map((k, v) => BinaryBExpr(BVEQ, SpecGamma(k).resolveSpec, v))
  private val inits = spec.inits.map((k, v) => BinaryBExpr(BVEQ, k.resolveSpec, BitVecLiteral(v.value, k.size)))

  def translate: BProgram = {
    val procedures = program.functions.map(f => translate(f))
    val globalDecls = procedures.flatMap(p => p.globals).map(b => BVarDecl(b)).distinct

    val globalConsts: List[BDeclaration] = globals.map(g => List(BVarDecl(g.toAddrVar), g.toAxiom)).toList.flatten

    val functionsUsed1: List[BFunction] = procedures.flatMap(p => p.functionOps).map(p => functionOpToDefinition(p)).distinct

    val rgProcs = genRely(relies) :+ BProcedure("guarantee_reflexive", List(), List(), List(), List(), Set(), guaranteesReflexive.map(g => Assert(g)))

    val functionsUsed2 = rgProcs.flatMap(p => p.functionOps).map(p => functionOpToDefinition(p)).distinct
    val functionsUsed3 = functionsUsed1.flatMap(p => p.functionOps).map(p => functionOpToDefinition(p)).distinct
    val functionsUsed = (functionsUsed1 ++ functionsUsed2 ++ functionsUsed3).distinct

    val declarations = globalDecls ++ globalConsts ++ functionsUsed ++ rgProcs ++ procedures
    avoidReserved(BProgram(declarations))
  }

  def genRely(relies: List[BExpr]): List[BProcedure] = {
    val mem = MapVar("mem", MapType(BitVec(64), BitVec(8)), Scope.Global)
    val Gamma_mem = MapVar("Gamma_mem", MapType(BitVec(64), BoolType), Scope.Global)
    val i = BVariable("i", BitVec(64), Scope.Local)
    val rely2 = ForAll(List(i), BinaryBExpr(BoolIMPLIES, BinaryBExpr(BVEQ, MapAccess(mem, i), Old(MapAccess(mem, i))), BinaryBExpr(BVEQ, MapAccess(Gamma_mem, i), Old(MapAccess(Gamma_mem, i)))))
    val ensures = List(rely2) ++ relies
    val relyProc = BProcedure("rely", List(), List(), ensures, List(), Set(mem, Gamma_mem), List())
    val relyTransitive = BProcedure("rely_transitive", List(), List(), relies, List(), Set(mem, Gamma_mem), List(ProcedureCall("rely", List(), List()), ProcedureCall("rely", List(), List())))
    val relyReflexive = BProcedure("rely_reflexive", List(), List(), List(), List(), Set(), reliesReflexive.map(r => Assert(r)))
    List(relyProc, relyTransitive, relyReflexive)
  }

  def functionOpToDefinition(f: FunctionOp): BFunction = {
    f match {
      case b: BVFunctionOp => BFunction(b.name, b.bvbuiltin, b.in, b.out, None)
      case m: MemoryLoad =>
        val memVar = MapVar("memory", m.memory.getType, Scope.Parameter)
        val indexVar = BParam("index", m.memory.getType.param)
        val in = List(memVar, indexVar)
        val out = BParam(BitVec(m.bits))
        val accesses: Seq[MapAccess] = for (i <- 0 until m.accesses) yield {
          if (i == 0) {
            MapAccess(memVar, indexVar)
          } else {
            MapAccess(memVar, BinaryBExpr(BVADD, indexVar, BitVecLiteral(i, m.addressSize)))
          }
        }
        val accessesEndian = m.endian match {
          case Endian.BigEndian    => accesses.reverse
          case Endian.LittleEndian => accesses
        }

        val body: BExpr = accessesEndian.tail.foldLeft(accessesEndian.head) {
          (concat: BExpr, next: MapAccess) => BinaryBExpr(BVCONCAT, next, concat)
        }

        BFunction(m.fnName, "", in, out, Some(body))
      case g: GammaLoad =>
        val gammaMapVar = MapVar("gammaMap", g.gammaMap.getType, Scope.Parameter)
        val indexVar = BParam("index", g.gammaMap.getType.param)
        val in = List(gammaMapVar, indexVar)
        val out = BParam(BoolType)
        val accesses: Seq[MapAccess] = for (i <- 0 until g.accesses) yield {
          if (i == 0) {
            MapAccess(gammaMapVar, indexVar)
          } else {
            MapAccess(gammaMapVar, BinaryBExpr(BVADD, indexVar, BitVecLiteral(i, g.addressSize)))
          }
        }

        val body: BExpr = accesses.tail.foldLeft(accesses.head) {
          (and: BExpr, next: MapAccess) => BinaryBExpr(BoolAND, next, and)
        }

        BFunction(g.fnName, "", in, out, Some(body))
      case m: MemoryStore =>
        val memVar = MapVar("memory", m.memory.getType, Scope.Parameter)
        val indexVar = BParam("index", m.memory.getType.param)
        val valueVar = BParam("value", BitVec(m.bits))
        val in = List(memVar, indexVar, valueVar)
        val out = BParam(m.memory.getType)
        val indices: Seq[BExpr] = for (i <- 0 until m.accesses) yield {
          if (i == 0) {
            indexVar
          } else {
            BinaryBExpr(BVADD, indexVar, BitVecLiteral(i, m.addressSize))
          }
        }
        val values: Seq[BExpr] = for (i <- 0 until m.accesses) yield {
          BVExtract((i + 1) * m.valueSize, i * m.valueSize, valueVar)
        }
        val valuesEndian = m.endian match {
          case Endian.BigEndian    => values.reverse
          case Endian.LittleEndian => values
        }
        val indiceValues = for (i <- 0 until m.accesses) yield {
          (indices(i), valuesEndian(i))
        }

        val body: MapUpdate = indiceValues.tail.foldLeft(MapUpdate(memVar, indices.head, valuesEndian.head)) {
          (update: MapUpdate, next: (BExpr, BExpr)) => MapUpdate(update, next._1, next._2)
        }

        BFunction(m.fnName, "", in, out, Some(body))
      case g: GammaStore =>
        val gammaMapVar = MapVar("gammaMap", g.gammaMap.getType, Scope.Parameter)
        val indexVar = BParam("index", g.gammaMap.getType.param)
        val valueVar = BParam("value", BoolType)
        val in = List(gammaMapVar, indexVar, valueVar)
        val out = BParam(g.gammaMap.getType)

        val indices: Seq[BExpr] = for (i <- 0 until g.accesses) yield {
          if (i == 0) {
            indexVar
          } else {
            BinaryBExpr(BVADD, indexVar, BitVecLiteral(i, g.addressSize))
          }
        }
        val values: Seq[BExpr] = for (i <- 0 until g.accesses) yield {
          valueVar
        }
        val indiceValues = for (i <- 0 until g.accesses) yield {
          (indices(i), values(i))
        }

        val body: MapUpdate = indiceValues.tail.foldLeft(MapUpdate(gammaMapVar, indices.head, values.head)) {
          (update: MapUpdate, next: (BExpr, BExpr)) => MapUpdate(update, next._1, next._2)
        }

        BFunction(g.fnName, "", in, out, Some(body))
      case l: L =>
        val memoryVar = BParam("memory", l.memory.getType)
        val indexVar = BParam("index", l.index.getType)
        val body: BExpr = LPreds.keys.foldLeft(FalseLiteral) {
          (ite: BExpr, next: SpecGlobal) => {
            val guard = BinaryBExpr(BoolEQ, indexVar, next.toAddrVar)
            val LPred = LPreds(next)
              /*if (controlled.contains(next)) {
              FunctionCall(s"L_${next.name}", List(l.memory), BoolType)
            } else {
              LPreds(next)
            } */
            IfThenElse(guard, LPred, ite)
          }
        }
        BFunction("L", "", List(memoryVar, indexVar), BParam(BoolType), Some(body))
    }
  }

  def translate(f: FunctionNode): BProcedure = {
    val in = f.in.flatMap(i => List(BParam(i.name, BitVec(i.size)), BParam(s"Gamma_${i.name}", BoolType)))
    val out = f.out.flatMap(o => List(BParam(o.name, BitVec(o.size)), BParam(s"Gamma_${o.name}", BoolType)))
    val returns = f.out.map(p => outParamToAssign(p))
    val body = f.blocks.map(b => translate(b, returns))
    val modifies = body.flatMap(b => b.modifies).toSet
    val requires: List[BExpr] = if (f.name == "main") {
      gammaInits.toList ++ inits.toList
    } else {
      List()
    }
    val inInits = if (body.isEmpty) {
      List()
    } else {
      f.in.map(p => inParamToAssign(p))
    }

    BProcedure(f.name, in, out, List(), requires, modifies, inInits ++ body)
  }

  private def outParamToAssign(p: Parameter): AssignCmd = {
    val param = BParam(p.name, BitVec(p.size))
    val register = p.value.toBoogie
    val paramGamma = BParam(s"Gamma_${p.name}", BoolType)
    val registerGamma = p.value.toGamma
    val assigned = if (p.size > p.value.size) {
      BVZeroExtend(p.size - p.value.size, register)
    } else if (p.size < p.value.size) {
      BVExtract(p.size, 0, register)
    } else {
      register
    }
    AssignCmd(List(param, paramGamma), List(assigned, registerGamma))
  }

  private def inParamToAssign(p: Parameter): AssignCmd = {
    val param = BParam(p.name, BitVec(p.size))
    val register = p.value.toBoogie
    val paramGamma = BParam(s"Gamma_${p.name}", BoolType)
    val registerGamma = p.value.toGamma
    val assigned = if (p.size > p.value.size) {
      BVExtract(p.value.size, 0, param)
    } else if (p.size < p.value.size) {
      BVZeroExtend(p.value.size - p.size, param)
    } else {
      param
    }
    AssignCmd(List(register, registerGamma), List(assigned, paramGamma))
  }

  def translate(b: Block, returns: List[BCmd]): BBlock = {
    // at some point we may want to take into account the instruction borders
    val statements = b.instructions.flatMap(_.statements)
    val cmds = statements.flatMap(s => translate(s, returns))
    BBlock(b.label, cmds)
  }

  def translate(s: Statement, returns: List[BCmd]): List[BCmd] = s match {
    case d: DirectCall =>
      val functionHeader = program.getFunction(d.target) match {
        case Some(s) => s
        case None => throw new Exception("trying to call non-existent procedure " + d.target)
      }
      val call = coerceProcedureCall(d.target, functionHeader.in, functionHeader.out)
      val returnTarget = d.returnTarget match {
        case Some(r) => List(GoToCmd(r))
        case None => List(Assume(FalseLiteral))
      }
      d.condition match {
        case l: Literal if l.value > BigInt(0) =>
          call ++ returnTarget
        case _ =>
          val guard = coerceToBool(d.condition.toBoogie)
          val guardGamma = d.condition.toGamma
          List(Assert(guardGamma), IfCmd(guard, call ++ returnTarget))
      }
    case i: IndirectCall =>
      val returnTarget = i.returnTarget match {
        case Some(r) => List(GoToCmd(r))
        case None => List(Assume(FalseLiteral))
      }
      val call = if (i.target.name == "R30") {
        returns :+ ReturnCmd
      } else {
        List(Comment(s"TODO: call ${i.target.name}")) ++ returnTarget // TODO determine call target
      }
      i.condition match {
        case l: Literal if l.value > BigInt(0) =>
          call
        case _ =>
          val guard = coerceToBool(i.condition.toBoogie)
          val guardGamma = i.condition.toGamma
          List(Assert(guardGamma), IfCmd(guard, call))
      }
    case g: GoTo =>
      g.condition match {
        case l: Literal if l.value > BigInt(0) =>
          List(GoToCmd(g.target))
        case _ =>
          val guard = coerceToBool(g.condition.toBoogie)
          val guardGamma = g.condition.toGamma
          List(Assert(guardGamma), IfCmd(guard, List(GoToCmd(g.target))))
      }
    case m: MemAssign =>
      val lhs = m.lhs.toBoogie
      val rhs = m.rhs.toBoogie
      val lhsGamma = m.lhs.toGamma
      val rhsGamma = m.rhs.toGamma
      val store = AssignCmd(List(lhs, lhsGamma), List(rhs, rhsGamma))
      if (m.lhs.name == "stack") {
        List(store)
      } else {
        val rely = ProcedureCall("rely", List(), List())
        val gammaValueCheck = Assert(BinaryBExpr(BoolIMPLIES, L(lhs, rhs.index), m.rhs.value.toGamma))
        val oldAssigns = guaranteeOldVars.map(g => AssignCmd(g.toOldVar, MemoryLoad(lhs, g.toAddrVar, Endian.LittleEndian, g.size))).toList
        val oldGammaAssigns = controlled.map(g => AssignCmd(g.toOldGamma, BinaryBExpr(BoolOR, GammaLoad(lhsGamma, g.toAddrVar, g.size, g.size / m.lhs.valueSize), L(lhs, g.toAddrVar))))
        val secureUpdate = for (c <- controls.keys) yield {
          val addrCheck = BinaryBExpr(BVEQ, rhs.index, c.toAddrVar)
          val checks = controls(c).map(v => BinaryBExpr(BoolIMPLIES, L(lhs, v.toAddrVar), v.toOldGamma)).toList
          val checksAnd = if (checks.size > 1) {
            checks.tail.foldLeft(checks.head)((next: BExpr, ands: BExpr) => BinaryBExpr(BoolAND, next, ands))
          } else {
            checks.head
          }
          Assert(BinaryBExpr(BoolIMPLIES, addrCheck, checksAnd))
        }
        val guaranteeChecks = guarantees.map(v => Assert(v))
        (List(rely, gammaValueCheck) ++ oldAssigns ++ oldGammaAssigns :+ store) ++ secureUpdate ++ guaranteeChecks
      }
    case l: LocalAssign =>
      val lhs = l.lhs.toBoogie
      val rhs = l.rhs.toBoogie
      val lhsGamma = l.lhs.toGamma
      val rhsGamma = l.rhs.toGamma
      val assign = AssignCmd(List(lhs, lhsGamma), List(rhs, rhsGamma))
      if (rhs.functionOps.collect { case m: MemoryLoad => m}.nonEmpty) {
        List(ProcedureCall("rely", List(), List()), assign)
      } else {
        List(assign)
      }

  }

  def coerceProcedureCall(target: String, in: List[Parameter], out: List[Parameter]): List[BCmd] = {
    val params = for (i <- in) yield {
      val register = i.value.toBoogie
      val registerGamma = i.value.toGamma
      if (i.value.size > i.size) {
        List(BVExtract(i.size, 0, register), registerGamma)
      } else if (i.value.size < i.size) {
        List(BVZeroExtend(i.size - i.value.size, register), registerGamma)
      } else {
        List(register, registerGamma)
      }
    }
    val outTemp = for (o <- out.indices) yield {
      BVariable(s"#temp$o", BitVec(out(o).size), Scope.Local)
    }
    val outTempGamma = for (o <- out.indices) yield {
      BVariable(s"Gamma_#temp$o", BoolType, Scope.Local)
    }
    val outRegisters = out.map(o => o.value.toBoogie)
    val outRegisterGammas = out.map(o => o.value.toGamma)
    val outAssigned = for (o <- out.indices if out(o).value.size != out(o).size) yield {
      val regSize = out(o).value.size
      val paramSize = out(o).size
      if (regSize > paramSize) {
        AssignCmd(List(outRegisters(o), outRegisterGammas(o)), List(BVZeroExtend(regSize - paramSize, outTemp(o)), outTempGamma(o)))
      } else {
        AssignCmd(List(outRegisters(o), outRegisterGammas(o)), List(BVExtract(regSize, 0, outTemp(o)), outTempGamma(o)))
      }
    }
    val returned = for (o <- out.indices) yield {
      if (out(o).value.size == out(o).size) {
        List(outRegisters(o), outRegisterGammas(o))
      } else {
        List(outTemp(o), outTempGamma(o))
      }
    }
    List(ProcedureCall(target, returned.flatten.toList, params.flatten)) ++ outAssigned
  }

  def coerceToBool(e: BExpr): BExpr = e.getType match {
    case BoolType => e
    case bv: BitVec => BinaryBExpr(BVNEQ, e, BitVecLiteral(0, bv.size))
    case _ => ???
  }

  def stripUnreachableFunctions(externalNames: Set[String]): BoogieTranslator = {
    val functionToChildren = program.functions.map(f => f.name -> f.calls).toMap
    val reachableFunctionNames = reachableFrom("main", functionToChildren, Set("main"))
    val reachableFunctions = program.functions.filter(f => reachableFunctionNames.contains(f.name))
    val externalsStubbed = reachableFunctions.map {
      case f: FunctionNode if externalNames.contains(f.name) => f.copy(blocks = List())
      case f: _ => f
    }
    copy(program = program.copy(functions = externalsStubbed))
  }

  private def reachableFrom(next: String, functionToChildren: Map[String, Set[String]], reached: Set[String]): Set[String] = {
    val reachable = functionToChildren(next) -- reached
    reached ++ reachable.flatMap(s => reachableFrom(s, functionToChildren, reachable ++ reached))
  }

  private val reserved = Set("free")

  def avoidReserved(program: BProgram): BProgram = {
    program.replaceReserved(reserved)
  }
}