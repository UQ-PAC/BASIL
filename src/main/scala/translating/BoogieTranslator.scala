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
    val out = f.out.map(i => BParam(i.name, BitVec(i.size)))
    val returns = f.out.map(i => AssignCmd(BParam(i.name, BitVec(i.size)), i.register.toBoogie)) // TODO make sizes match
    val body = f.blocks.map(b => translate(b, returns))
    val modifies = body.flatMap(b => b.modifies).toSet
    val inits = f.in.map(i => AssignCmd(i.register.toBoogie, BParam(i.name, BitVec(i.size)))) // TODO make sizes match

    BProcedure(f.name, in, out, List(), List(), modifies, inits ++ body)
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
      val params = functionHeader.in.map(p => p.register.toBoogie)
      val out = functionHeader.out.map(p => p.register.toBoogie)
      val call = List(ProcedureCall(d.target, out, params))
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

  def coerceToBool(e: BExpr): BExpr = e.getType match {
    case BoolType => e
    case bv: BitVec => BinaryBExpr(BVNEQ, e, BitVecLiteral(0, bv.size))
    case _ => ???
  }
}