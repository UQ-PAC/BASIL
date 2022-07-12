package translating

import astnodes._
import boogie._

case class BoogieTranslator(program: Program) {
  def translate: BProgram = {
    val procedures = program.functions.map(f => translate(f))
    val globals = procedures.flatMap(p => p.globals).map(b => BVarDecl(b)).distinct
    val functionsUsed: List[BFunction] = procedures.flatMap(p => p.functionOps).distinct.map(p => functionOpToDefinition(p))

    val declarations = globals ++ functionsUsed ++ procedures
    avoidReserved(BProgram(declarations))
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
            MapAccess(memVar, BinaryBExpr(BVADD, indexVar, BitVecLiteral(i, m.memory.getType.param)))
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
            MapAccess(gammaMapVar, BinaryBExpr(BVADD, indexVar, BitVecLiteral(i, g.gammaMap.getType.param)))
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
            BinaryBExpr(BVADD, indexVar, BitVecLiteral(i, m.memory.getType.param))
          }
        }
        val values: Seq[BExpr] = for (i <- 0 until m.accesses) yield {
          BVExtract((i + 1) * (m.accesses / m.bits), i * (m.accesses / m.bits), valueVar)
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
        val valueVar = BParam("value", BitVec(g.bits))
        val in = List(gammaMapVar, indexVar, valueVar)
        val out = BParam(g.gammaMap.getType)

        val indices: Seq[BExpr] = for (i <- 0 until g.accesses) yield {
          if (i == 0) {
            indexVar
          } else {
            BinaryBExpr(BVADD, indexVar, BitVecLiteral(i, g.bits))
          }
        }
        val values: Seq[BExpr] = for (i <- 0 until g.accesses) yield {
          BVExtract((i + 1) * (g.accesses / g.bits), i * (g.accesses / g.bits), valueVar)
        }
        val indiceValues = for (i <- 0 until g.accesses) yield {
          (indices(i), values(i))
        }

        val body: MapUpdate = indiceValues.tail.foldLeft(MapUpdate(gammaMapVar, indices.head, values.head)) {
          (update: MapUpdate, next: (BExpr, BExpr)) => MapUpdate(update, next._1, next._2)
        }

        BFunction(g.fnName, "", in, out, Some(body))
    }
  }

  def translate(f: FunctionNode): BProcedure = {
    val in = f.in.flatMap(i => List(BParam(i.name, BitVec(i.size)), BParam(s"Gamma_${i.name}", BoolType)))
    val out = f.out.flatMap(o => List(BParam(o.name, BitVec(o.size)), BParam(s"Gamma_${o.name}", BoolType)))
    val returns = f.out.map(p => outParamToAssign(p))
    val body = f.blocks.map(b => translate(b, returns))
    val modifies = body.flatMap(b => b.modifies).toSet
    val inits = if (body.isEmpty) {
      List()
    } else {
      f.in.map(p => inParamToAssign(p))
    }

    BProcedure(f.name, in, out, List(), List(), modifies, inits ++ body)
  }

  private def outParamToAssign(p: Parameter): AssignCmd = {
    val param = BParam(p.name, BitVec(p.size))
    val register = p.register.toBoogie
    val paramGamma = BParam(s"Gamma_${p.name}", BoolType)
    val registerGamma = p.register.toGamma
    val assigned = if (p.size > p.register.size) {
      BVZeroExtend(p.size - p.register.size, register)
    } else if (p.size < p.register.size) {
      BVExtract(p.size, 0, register)
    } else {
      register
    }
    AssignCmd(List(param, paramGamma), List(assigned, registerGamma))
  }

  private def inParamToAssign(p: Parameter): AssignCmd = {
    val param = BParam(p.name, BitVec(p.size))
    val register = p.register.toBoogie
    val paramGamma = BParam(s"Gamma_${p.name}", BoolType)
    val registerGamma = p.register.toGamma
    val assigned = if (p.size > p.register.size) {
      BVExtract(p.register.size, 0, param)
    } else if (p.size < p.register.size) {
      BVZeroExtend(p.register.size - p.size, param)
    } else {
      param
    }
    AssignCmd(List(register, registerGamma), List(assigned, registerGamma))
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
          List(IfCmd(guard, call ++ returnTarget))
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
          List(IfCmd(guard, call))
      }
    case g: GoTo =>
      g.condition match {
        case l: Literal if l.value > BigInt(0) =>
          List(GoToCmd(g.target))
        case _ =>
          val guard = coerceToBool(g.condition.toBoogie)
          List(IfCmd(guard, List(GoToCmd(g.target))))
      }
    case m: MemAssign =>
      val lhs = m.lhs.toBoogie
      val rhs = m.rhs.toBoogie
      val lhsGamma = m.lhs.toGamma
      val rhsGamma = m.rhs.toGamma
      List(AssignCmd(List(lhs, lhsGamma), List(rhs, rhsGamma)))
      /*
      val rhsBoogie = m.rhs.toBoogie
      val lhss = m.lhs.boogieAccesses
      if (lhss.size > 1) {
        val tempLocal = rhsBoogie match {
          case b: BVar => b
          case _ => BVariable("#temp", BitVec(m.rhs.size), true, false)
        }
        val tempAssign = if (rhsBoogie == tempLocal) {
          List()
        } else {
          List(AssignCmd(tempLocal, rhsBoogie))
        }
        val width = m.lhs.memory.valueSize
        val assigns = for (i <- lhss.indices) yield {
          MapAssignCmd(lhss(i), BVExtract((i + 1) * width, i * width, tempLocal))
        }
        tempAssign ++ assigns
      } else {
        List(MapAssignCmd(lhss.head, rhsBoogie))
      }
      */
    case l: LocalAssign =>
      val lhs = l.lhs.toBoogie
      val rhs = l.rhs.toBoogie
      val lhsGamma = l.lhs.toGamma
      val rhsGamma = l.rhs.toGamma
      List(AssignCmd(List(lhs, lhsGamma), List(rhs, rhsGamma)))
  }

  def coerceProcedureCall(target: String, in: List[Parameter], out: List[Parameter]): List[BCmd] = {
    val params = for (i <- in) yield {
      val register = i.register.toBoogie
      if (i.register.size > i.size) {
        BVExtract(i.size, 0, register)
      } else if (i.register.size < i.size) {
        BVZeroExtend(i.size - i.register.size, register)
      } else {
        register
      }
    }
    val outTemp = for (o <- out.indices) yield {
      BVariable(s"#temp$o", BitVec(out(o).size), Scope.Local)
    }
    val outRegisters = out.map(o => o.register.toBoogie)
    val outAssigned = for (o <- out.indices if out(o).register.size != out(o).size) yield {
      val regSize = out(o).register.size
      val paramSize = out(o).size
      if (regSize > paramSize) {
        AssignCmd(outRegisters(o), BVZeroExtend(regSize - paramSize, outTemp(o)))
      } else {
        AssignCmd(outRegisters(o), BVExtract(regSize, 0, outTemp(o)))
      }
    }
    val returned = for (o <- out.indices) yield {
      if (out(o).register.size == out(o).size) {
        outRegisters(o)
      } else {
        outTemp(o)
      }
    }
    List(ProcedureCall(target, returned.toList, params)) ++ outAssigned
  }

  def coerceToBool(e: BExpr): BExpr = e.getType match {
    case BoolType => e
    case bv: BitVec => BinaryBExpr(BVNEQ, e, BitVecLiteral(0, bv.size))
    case _ => ???
  }

  def stripUnreachableFunctions(externalNames: Set[String]): Program = {
    val functionToChildren = program.functions.map(f => f.name -> f.calls).toMap
    val reachableFunctionNames = reachableFrom("main", functionToChildren, Set("main"))
    val reachableFunctions = program.functions.filter(f => reachableFunctionNames.contains(f.name))
    val externalsStubbed = reachableFunctions.map {
      case f: FunctionNode if externalNames.contains(f.name) => f.copy(blocks = List())
      case f: _ => f
    }
    program.copy(functions = externalsStubbed)
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