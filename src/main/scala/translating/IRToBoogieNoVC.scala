package translating
import boogie.*
import ir.*
import ir.cilvisitor.*

import scala.collection.mutable

object BoogieTranslator {

  def translateType(e: IRType): BType = e match {
    case BoolType => BoolBType
    case IntType => IntBType
    case BitVecType(s) => BitVecBType(s)
    case MapType(p, r) => MapBType(translateType(p), translateType(r))
    case CustomSort(s) => CustomBType(s)
  }

  def translateVar(e: Variable): BVar = e match {
    case Register(n, s) => BVariable(n, translateType(e.getType), Scope.Global)
    case v: LocalVar => BVariable(v.name, translateType(v.getType), Scope.Local)
    case v: GlobalVar => BVariable(v.name, translateType(v.getType), Scope.Global)
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

  def slToBoogie(e: List[Statement]) = e.flatMap(translateStatement)

  def captureStateStatement(stateName: String): BAssume = {
    BAssume(TrueBLiteral, None, List(BAttribute("captureState", Some(s"\"$stateName\""))))
  }

  def translateStatement(s: Statement): Iterable[BCmd] = s match {
    case m: NOP => Seq()
    case m: SimulAssign if m.assignments.isEmpty => Seq()
    case m: SimulAssign =>
      val a = m.assignments
      val lhs = a.map(_._1).map(translateVar).toList
      val rhs = a.map(_._2).map(translateExpr).toList
      Seq(AssignCmd(lhs, rhs))
    case l: LocalAssign =>
      val lhs: BVar = translateVar(l.lhs)
      val rhs = translateExpr(l.rhs)
      Seq(AssignCmd(List(lhs), List(rhs)))
    case l: MemoryAssign =>
      val lhs: BVar = translateVar(l.lhs)
      val rhs = translateExpr(l.rhs)
      Seq(AssignCmd(List(lhs), List(rhs)))
    case a: Assert =>
      val body = translateExpr(a.body)
      Seq(BAssert(body, a.comment))
    case a: Assume =>
      val body = translateExpr(a.body)
      Seq(BAssume(body, a.comment))
    case m: MemoryStore =>
      val lhs = m.mem.toBoogie
      val rhs = BMemoryStore(m.mem.toBoogie, m.index.toBoogie, m.value.toBoogie, m.endian, m.size)
      val store = AssignCmd(List(lhs), List(rhs))
      Seq(store)
    case m: MemoryLoad =>
      val lhs = m.lhs.toBoogie
      val rhs = BMemoryLoad(m.mem.toBoogie, m.index.toBoogie, m.endian, m.size)
      val assign = AssignCmd(List(lhs), List(rhs))
      // add rely call if is a non-stack load
      Seq(assign)
    case d: DirectCall =>
      Seq(
        BProcedureCall(
          d.target.name,
          d.outParams.values.toSeq.map(_.toBoogie),
          d.actualParams.values.toSeq.map(_.toBoogie)
        )
      )
    case f: IndirectCall => Seq(BAssert(FalseBLiteral, Some("IndirectCall" + f.target.toString)))
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

  def translateProc(
    freeRequires: Iterable[BExpr] = Seq(),
    freeEnsures: Iterable[BExpr] = Seq(),
    extraRequires: Iterable[BExpr] = Seq(),
    extraEnsures: Iterable[BExpr] = Seq()
  )(e: Procedure): BProcedure = {

    val body: List[BCmdOrBlock] =
      (e.entryBlock.view ++ e.blocks.filterNot(x => e.entryBlock.contains(x))).map(x => translateBlock(x)).toList

    val inparams = e.formalInParam.toList.map(_.toBoogie)
    val outparams = e.formalOutParam.toList.map(_.toBoogie)

    BProcedure(
      e.name,
      inparams,
      outparams,
      e.ensures ++ e.ensuresExpr.map(translateExpr) ++ extraEnsures,
      e.requires ++ e.requiresExpr.map(translateExpr) ++ extraRequires,
      List(),
      List(),
      freeEnsures.toList,
      freeRequires.toList,
      e.modifies.map(translateGlobal).toSet,
      body
    )

  }

  private def functionOpToDecl(functionOps: Iterable[BDeclaration]): Iterable[BDeclaration] = {
    var decls = Set[BDeclaration]()
    var oldFops = Set[FunctionOp]()
    var fops: Set[FunctionOp] = functionOps.flatMap(_.functionOps).toSet

    while (oldFops != fops) {
      oldFops = fops
      decls = fops.flatMap {
        case f: BasilIRFunctionOp => Seq(genFunctionOpDefinition(f))
        case f => throw Exception(s"function op not supported on direct translation mode : $f")
      }
      val newOps = decls.flatMap(_.functionOps)
      fops = fops ++ newOps
    }

    decls
  }

  def translateProg(p: Program, spec: Option[specification.Specification], fname: String = "output.bpl") = {
    p.setModifies(Map())

    val procSpecs = spec.toSeq.flatMap(_.subroutines).map(s => s.name -> s).toMap

    val vvis = FindVars()
    visit_prog(vvis, p)

    for (proc <- p.procedures) {
      val vvis = FindVars()
      visit_proc(vvis, proc)
      proc.modifies.addAll(vvis.globals)
    }

    val typeDecls = vvis.typeDecls
      .map(_.toBoogie)
      .collect { case b @ CustomBType(_) =>
        b
      }
      .map(BTypeDecl(_))

    val globalDecls = p.declarations.map(_.toBoogie)

    val readOnlySections = p.usedMemory.values.filter(_.readOnly)
    val readOnlyMemory = memoryToConditionCoalesced(readOnlySections)
    val initialSections = p.usedMemory.values.filter(!_.readOnly)
    val initialMemory = memoryToConditionCoalesced(initialSections)

    val memGlobals = (readOnlyMemory ++ initialMemory).flatMap(_.globals).map(BVarDecl(_))
    val globalVarDecls = vvis.globals.map(translateGlobal).map(BVarDecl(_)) ++ memGlobals

    val procs = p.procedures.map(proc => {
      val requires = procSpecs.get(proc.procName).toSeq.flatMap(_.requires)
      val ensures = procSpecs.get(proc.procName).toSeq.flatMap(_.ensures)
      if (p.mainProcedure eq proc) {
        translateProc(initialMemory ++ readOnlyMemory, Seq(), requires, ensures)(proc)
      } else {
        translateProc(readOnlyMemory, Seq(), requires, ensures)(proc)
      }
    })

    val functionOpDefinitions = functionOpToDecl(globalDecls ++ globalVarDecls ++ procs)
    val decls = globalVarDecls.toList ++ globalDecls ++ functionOpDefinitions ++ procs

    BProgram(typeDecls.toList ++ decls, fname)
  }

}

class FindVars extends CILVisitor {
  /*
   * Collect variables visited by performing performs DFS of call tree.
   */

  val vars = mutable.Set[Variable]()
  val mems = mutable.Set[Memory]()
  var procsSeen = mutable.Set[Procedure]()
  val typeDecls = mutable.Set[CustomSort]()

  override def vmem(m: Memory) = {
    mems += m
    SkipChildren()
  }

  override def vrvar(v: Variable) = {
    vars += v
    SkipChildren()
  }

  override def vproc(p: Procedure) = {
    procsSeen.add(p)
    DoChildren()
  }

  override def vlvar(v: Variable) = {
    vars += v
    SkipChildren()
  }

  override def vexpr(e: Expr) = {
    e.getType match {
      case s @ CustomSort(_) => typeDecls.add(s)
      case _ => ()
    }
    DoChildren()
  }

  override def vstmt(s: Statement) = {
    s match {
      case s: DirectCall if !procsSeen.contains(s.target) => visit_proc(this, s.target)
      case _ => ()
    }
    DoChildren()
  }

  def globals = (vars ++ mems).collect { case g: Global =>
    g
  }

  def locals = vars.collect { case v: LocalVar =>
    v
  }
}
