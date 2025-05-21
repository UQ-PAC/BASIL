package translating
import ir.*
import boogie.*
import specification.*
import scala.collection.mutable
import ir.cilvisitor.*

object BoogieTranslator {

  def translateType(e: IRType): BType = e match {
    case BoolType => BoolBType
    case IntType => IntBType
    case BitVecType(s) => BitVecBType(s)
    case MapType(p, r) => MapBType(translateType(p), translateType(r))
  }

  def translateVar(e: Variable): BVar = e match {
    case Register(n, s) => BVariable(n, translateType(e.getType), Scope.Global)
    case v: LocalVar => BVariable(v.name, translateType(v.getType), Scope.Local)
  }

  def translateMem(e: Memory): BMapVar =
    BMapVar(e.name, MapBType(BitVecBType(e.addressSize), BitVecBType(e.valueSize)), Scope.Global)

  def translateGlobal(g: Global) = {
    g match {
      case v: Variable => translateVar(v)
      case m: Memory => translateMem(m)
    }
  }

  def translateExpr(e: Expr): BExpr = e.toBoogie

  def slToBoogie(e: List[Statement]) = e.map(translateStatement)

  def captureStateStatement(stateName: String): BAssume = {
    BAssume(TrueBLiteral, None, List(BAttribute("captureState", Some(s"\"$stateName\""))))
  }

  def translateStatement(s: Statement): BCmd = s match {
    case m: NOP => BAssume(TrueBLiteral, Some("NOP"))
    case l: LocalAssign =>
      val lhs: BVar = translateVar(l.lhs)
      val rhs = translateExpr(l.rhs)
      AssignCmd(List(lhs), List(rhs))
    case l: MemoryAssign =>
      val lhs: BVar = translateVar(l.lhs)
      val rhs = translateExpr(l.rhs)
      AssignCmd(List(lhs), List(rhs))
    case a: Assert =>
      val body = translateExpr(a.body)
      BAssert(body, a.comment)
    case a: Assume =>
      val body = translateExpr(a.body)
      BAssume(body, a.comment)
    case m: MemoryStore =>
      val lhs = m.mem.toBoogie
      val rhs = BMemoryStore(m.mem.toBoogie, m.index.toBoogie, m.value.toBoogie, m.endian, m.size)
      val store = AssignCmd(List(lhs), List(rhs))
      store
    case m: MemoryLoad =>
      val lhs = m.lhs.toBoogie
      val rhs = BMemoryLoad(m.mem.toBoogie, m.index.toBoogie, m.endian, m.size)
      val assign = AssignCmd(List(lhs), List(rhs))
      // add rely call if is a non-stack load
      assign
    case d: DirectCall =>
      BProcedureCall(
        d.target.name,
        d.outParams.values.toSeq.map(_.toBoogie),
        d.actualParams.values.toSeq.map(_.toBoogie)
      )
    case f: IndirectCall => BAssert(FalseBLiteral, Some("IndirectCall" + f.target.toString))
  }

  def translateJump(j: Jump): List[BCmd] = {
    j match {
      case g: GoTo => List(GoToCmd(g.targets.map(_.label).toSeq))
      case r: Return =>
        if (r.outParams.nonEmpty) {
          val lhss = r.outParams.keys.toSeq
          val rhss = r.outParams.values.toSeq
          List(AssignCmd(lhss.map(_.toBoogie), rhss.map(_.toBoogie)), ReturnCmd)
        } else {
          List(ReturnCmd)
        }
      case u: Unreachable => List(BAssume(FalseBLiteral))
    }
  }

  def translateBlock(b: Block, unused: Unit = ()): BBlock = translateBlock(b)
  def translateBlock(b: Block): BBlock = {
    BBlock(b.label, slToBoogie(b.statements.toList) ++ translateJump(b.jump))
  }

  def translateProc(freeRequires: Iterable[BExpr] = Set(), freeEnsures: Iterable[BExpr] = Set())(
    e: Procedure
  ): BProcedure = {

    val locals = {
      val vars = FindVars()
      cilvisitor.visit_proc(vars, e)
      vars.locals
    }

    val body: List[BCmdOrBlock] =
      (e.entryBlock.view ++ e.blocks.filterNot(x => e.entryBlock.contains(x))).map(x => translateBlock(x)).toList

    val inparams = e.formalInParam.toList.map(_.toBoogie)
    val outparams = e.formalOutParam.toList.map(_.toBoogie)

    BProcedure(
      e.name,
      inparams,
      outparams,
      e.ensures ++ e.ensuresExpr.map(translateExpr),
      e.requires ++ e.requiresExpr.map(translateExpr),
      List(),
      List(),
      freeEnsures.toList,
      freeRequires.toList,
      e.modifies.map(translateGlobal).toSet,
      body
    )

  }

  private def functionOpToDecl(functionOps: Iterable[BDeclaration]): Iterable[BFunction] = {
    var decls = Set[BFunction]()
    var oldFops = Set[FunctionOp]()
    var fops: Set[FunctionOp] = functionOps.flatMap(_.functionOps).toSet

    while (oldFops != fops) {
      oldFops = fops
      decls = fops.map {
        case f: BasilIRFunctionOp => genFunctionOpDefinition(f)
        case f => throw Exception(s"function op not supported on direct translation mode : $f")
      }
      val newOps = decls.flatMap(_.functionOps)
      fops = fops ++ newOps
    }

    decls
  }

  def translateProg(p: Program, fname: String = "output.bpl") = {
    p.setModifies(Map())

    val vvis = FindVars()
    visit_prog(vvis, p)

    val readOnlySections = p.usedMemory.values.filter(_.readOnly)
    val readOnlyMemory = memoryToConditionCoalesced(readOnlySections)
    val initialSections = p.usedMemory.values.filter(!_.readOnly)
    val initialMemory = memoryToConditionCoalesced(initialSections)

    val memGlobals = (readOnlyMemory ++ initialMemory).flatMap(_.globals).map(BVarDecl(_))
    val globalVarDecls = vvis.globals.map(translateGlobal).map(BVarDecl(_)) ++ memGlobals

    val procs = p.procedures.map {
      case proc if p.mainProcedure eq proc => translateProc(initialMemory ++ readOnlyMemory)(proc)
      case proc => translateProc(readOnlyMemory)(proc)
    }

    val functionOpDefinitions = functionOpToDecl(globalVarDecls ++ procs)
    val decls = globalVarDecls.toList ++ functionOpDefinitions ++ procs

    BProgram(decls, fname)
  }

}

class FindVars extends CILVisitor {
  val vars = mutable.Set[Variable]()
  val mems = mutable.Set[Memory]()

  override def vmem(m: Memory) = {
    mems += m
    SkipChildren()
  }

  override def vrvar(v: Variable) = {
    vars += v
    SkipChildren()
  }
  override def vlvar(v: Variable) = {
    vars += v
    SkipChildren()
  }

  def globals = (vars ++ mems).collect { case g: Global =>
    g
  }

  def locals = vars.collect { case v: LocalVar =>
    v
  }
}
