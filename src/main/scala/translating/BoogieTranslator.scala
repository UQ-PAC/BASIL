package translating

import astnodes._
import boogie._

case class BoogieTranslator(program: Program) {
  def translate: BProgram = {
    val procedures = program.functions.map(f => translate(f))
    val globals = procedures.flatMap(p => p.modifies).map(b => BVarDecl(b)).distinct
    val functionsUsed = procedures.flatMap(p => p.bvFunctions).distinct

    val declarations = globals ++ functionsUsed ++ procedures
    BProgram(declarations)
  }

  def translate(f: FunctionNode): BProcedure = {
    val in = f.in.map(i => BParam(i.name, BitVec(i.size)))
    val out = f.out.map(o => BParam(o.name, BitVec(o.size)))
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
    if (p.size > p.register.size) {
      AssignCmd(param, BVZeroExtend(p.size - p.register.size, register))
    } else if (p.size < p.register.size) {
      AssignCmd(param, BVExtract(p.size, 0, register))
    } else {
      AssignCmd(param, register)
    }
  }

  private def inParamToAssign(p: Parameter): AssignCmd = {
    val param = BParam(p.name, BitVec(p.size))
    val register = p.register.toBoogie
    if (p.size > p.register.size) {
      AssignCmd(register, BVExtract(p.register.size, 0, param))
    } else if (p.size < p.register.size) {
      AssignCmd(register, BVZeroExtend(p.register.size - p.size, param))
    } else {
      AssignCmd(register, param)
    }
  }

  def translate(b: Block, returns: List[BCmd]): BBlock = {
    // TODO at some point we want to take into account the instruction borders
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
      val rhsBoogie = m.rhs.toBoogie
      val lhss = m.lhs.boogieAccesses
      if (lhss.size > 1) {
        val tempLocal = rhsBoogie match {
          case b: BVar => b
          case _ => BVariable("#temp", BitVec(m.rhs.size), Scope.Local)
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
    case l: LocalAssign => List(AssignCmd(l.lhs.toBoogie, l.rhs.toBoogie))
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
}