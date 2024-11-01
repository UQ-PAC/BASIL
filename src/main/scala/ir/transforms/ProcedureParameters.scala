package ir.transforms
import ir.cilvisitor.*
import ir.*
import scala.collection.mutable.ArrayBuffer
import scala.collection.{mutable, immutable}
import collection.immutable.SortedMap
import specification.Specification
import analysis.{TwoElement, TwoElementTop, TwoElementBottom}
import ir.CallGraph

case class FunSig(inArgs: List[Register], outArgs: List[Register]) 

def R(n: Int) = {
  Register(s"R$n", 64)
}

val builtinSigs : Map[String, FunSig] = Map(
  "#free" -> FunSig(List(R(0)), List(R(0))),
  "malloc" -> FunSig(List(R(0)), List(R(0))),
  "strlen" -> FunSig(List(R(0)), List(R(0))),
  "strchr" -> FunSig(List(R(0), R(1)), List(R(0))),
  "strlcpy" -> FunSig(List(R(0), R(1), R(2)), List(R(0))),
  "strlcat" -> FunSig(List(R(0), R(1), R(2)), List(R(0)))
  )

def fnsigToBinding(f: FunSig) = (f.inArgs.map(a => LocalVar(a.name + "_in", a.getType) -> LocalVar(a.name, a.getType)), 
  f.outArgs.map(a => LocalVar(a.name + "_out", a.getType) -> LocalVar(a.name, a.getType)))

def liftProcedureCallAbstraction(ctx: util.IRContext): util.IRContext = {

  val liveVars  =
  if (ctx.program.mainProcedure.blocks.nonEmpty && ctx.program.mainProcedure.returnBlock.isDefined && ctx.program.mainProcedure.entryBlock.isDefined) {
    analysis.InterLiveVarsAnalysis(ctx.program).analyze()
  } else {
    Map.empty
  }

  val params = inOutParams(ctx.program, liveVars)

  // functions for which we don't know their behaviour and assume they modify all registers
  val external = ctx.externalFunctions.map(_.name) ++ ctx.program.collect {
    case b: Procedure if b.blocks.isEmpty => b.name
  }

  val formalParams = SetFormalParams(params, external)
  visit_prog(formalParams, ctx.program)
  val actualParams = SetActualParams(formalParams.mappingInparam, formalParams.mappingOutparam, external)
  visit_prog(actualParams, ctx.program)

  ctx.copy(specification = specToProcForm(ctx.specification, formalParams.mappingInparam, formalParams.mappingOutparam))
}

def clearParams(p: Program) = {

  class RemoveCallParams extends CILVisitor {
    override def vstmt(s: Statement) = s match {
      case d: DirectCall =>
        ChangeTo(List(DirectCall(d.target, d.label, immutable.SortedMap.empty, immutable.SortedMap.empty)))
      case _ => SkipChildren()
    }
  }

  for (c <- p) {
    c match {
      case proc: Procedure => {
        proc.formalInParam.clear
        proc.formalOutParam.clear
        proc.inParamDefaultBinding = immutable.SortedMap.empty
        proc.outParamDefaultBinding = immutable.SortedMap.empty
      }
      case _ => ()
    }
  }

  visit_prog(RemoveCallParams(), p)
}

def collectVariables(p: Procedure): (Set[Variable], Set[Variable]) = {
  val lvars = p.blocks.toSet.flatMap(_.statements.flatMap(s => {
    s match {
      case Assign(l, _, _)        => Set(l)
      case DirectCall(t, o, _, _) => o.toSet.map(_._2)
      case _                      => Set()
    }
  })) ++ p.blocks
    .map(_.jump)
    .collect { case r: Return =>
      r.outParams.toSet.map(_._1)
    }
    .flatten
  val rvars = p.blocks.toSet.flatMap(_.statements.flatMap(s => {
    s match {
      case Assign(l, r, _)                => r.variables
      case Assume(l, _, _, _)             => l.variables
      case Assert(l, _, _)                => l.variables
      case MemoryAssign(m, i, v, _, _, _) => i.variables ++ v.variables
      case IndirectCall(l, _)             => Set(l)
      case DirectCall(t, o, l, _)         => l.toSet.flatMap(_._2.variables)
      case _                              => Set()
    }
  }))

  (lvars.toSet, rvars.toSet)
}

class SetFormalParams(
    val inoutparams: Map[Procedure, (Set[Variable], Set[Variable])],
    val externalFunctions: Set[String]
) extends CILVisitor {
  // expects programs to be in single return form

  var mappingOutparam = Map[Procedure, Map[LocalVar, Variable]]()
  var mappingInparam = Map[Procedure, Map[LocalVar, Variable]]()

  def externalIn: Map[LocalVar, Variable] = {
    (0 to 31).map(i => LocalVar(s"R${i}_in", BitVecType(64)) -> LocalVar(s"R$i", BitVecType(64))).toMap
  }
  def externalOut: Map[LocalVar, Variable] = {
    (0 to 31).map(i => LocalVar(s"R${i}_out", BitVecType(64)) -> LocalVar(s"R$i", BitVecType(64))).toMap
  }

  override def vproc(p: Procedure) = {
    if (externalFunctions.contains(p.name)) {



      p.formalInParam = mutable.SortedSet.from(externalIn.map(_._1))
      p.formalOutParam = mutable.SortedSet.from(externalOut.map(_._1))
      p.inParamDefaultBinding = immutable.SortedMap.from(externalIn)
      p.outParamDefaultBinding = immutable.SortedMap.from(externalOut)
      mappingInparam = mappingInparam.updated(p, externalIn)
      mappingOutparam = mappingOutparam.updated(p, externalOut)
      SkipChildren()
    } else {
      val (lvars, rvars) = collectVariables(p)

      val in = IRWalk.firstInProc(p)
      if (in.isDefined) {
        val inparams = inoutparams(p)._1.map(v => (LocalVar(v.name + "_in", v.getType), LocalVar(v.name, v.getType)))
        p.formalInParam.addAll(inparams.map(_._1))
        mappingInparam = mappingInparam.updated(p, inparams.toMap)
        p.inParamDefaultBinding = immutable.SortedMap.from(inparams)
      }

      // outparams is everything touched
      val outparams = inoutparams(p)._2.map(v => LocalVar(v.name + "_out", v.getType) -> LocalVar(v.name, v.getType))
      p.formalOutParam = mutable.SortedSet.from(outparams.map(_._1))
      mappingOutparam = mappingOutparam.updated(p, outparams.toMap)
      p.outParamDefaultBinding = immutable.SortedMap.from(outparams)

      SkipChildren()
    }
  }
}

object ReadWriteAnalysis {
  sealed trait RW {
    def getRWSet: Option[RWSet] = {
      this match {
        case Top      => None
        case r: RWSet => Some(r)
      }
    }
    def map[B](f: RW => RW) = {
      f(this)
    }
  }
  case class RWSet(reads: Set[Variable], writes: Set[Variable]) extends RW
  case object Top extends RW

  type st = Map[Procedure, RW]

  def addReads(r: Iterable[Variable])(i: RW) = {
    i match {
      case Top      => Top
      case i: RWSet => i.copy(reads = i.reads ++ r)
    }
  }
  def addWrites(w: Iterable[Variable])(i: RW) = {
    i match {
      case Top      => Top
      case i: RWSet => i.copy(writes = i.writes ++ w)
    }
  }

  def join(a: RW, b: RW): RW = {
    (a, b) match {
      case (Top, _)             => Top
      case (_, Top)             => Top
      case (a: RWSet, b: RWSet) => RWSet(a.reads ++ b.reads, a.writes ++ b.writes)
    }
  }

  def processProc(state: st, p: Procedure): RW = {
    p.foldLeft(state(p))((ir, s) => {
      s match {
        case s: Assign => {
          ir.map(addWrites(Seq(s.lhs)))
            .map(addReads(s.rhs.variables))
        }
        case s: Return => {
          ir.map(addReads(s.outParams.flatMap(_._2.variables)))
        }
        case s: MemoryAssign => {
          ir.map(addReads(s.index.variables ++ s.value.variables))
        }
        case s: DirectCall => {
          ir.map(x => join(x, state(s.target)))
            .map(addReads(s.actualParams.flatMap(_._2.variables)))
            .map(addWrites(s.outParams.flatMap(_._2.variables)))
        }
        case s: IndirectCall => Top
        case s: Assert       => ir.map(addReads(s.body.variables))
        case s: Assume       => ir.map(addReads(s.body.variables))
        case p: Procedure    => ir
        case b: Block        => ir
        case b: NOP          => ir
        case b: Unreachable  => ir
        case b: GoTo         => ir
      }
    })
  }

  def readWriteSets(p: Program): Map[Procedure, Option[RWSet]] = {
    var state: st = Map[Procedure, RW]().withDefaultValue(RWSet(Set(), Set()))
    val worklist = mutable.Stack.from(p.procedures)

    while (worklist.nonEmpty) {
      val proc = worklist.pop
      val o = state(proc)
      val n = processProc(state, proc)
      if (o != n) {
        worklist.addAll(CallGraph.pred(proc))
        worklist.addAll(CallGraph.succ(proc))
        state = state.updated(proc, n)
      }
    }

    state.map(x => (x._1, x._2.getRWSet))
  }
}

def inOutParams(
    p: Program,
    interLiveVarsResults: Map[CFGPosition, Map[Variable, TwoElement]]
): Map[Procedure, (Set[Variable], Set[Variable])] = {
  val overapprox = (0 to 31).map(i => Register(s"R${i}", 64)).toSet[Variable]
  // in: live at entry & in procedure read set

  val readWrites = ReadWriteAnalysis.readWriteSets(p: Program).collect {
    case (p, None)    => (p, ReadWriteAnalysis.RWSet(overapprox, overapprox))
    case (p, Some(x)) => (p, x)
  }

  val procEnd = p.procedures.map { case p =>
    p -> p.returnBlock.getOrElse(p)
  }.toMap

  val lives : Map[Procedure, (Set[Variable], Set[Variable])] = p.procedures
    .map(p => {
      val in = (interLiveVarsResults.get(p))
      val out = (interLiveVarsResults.get(procEnd(p)))

      def toLiveSet(p: Option[Map[Variable, TwoElement]]): Set[Variable] = {
        p.map(p => {
          p.collect { case (v, TwoElementTop) =>
            v
          }.toSet
        }).getOrElse(overapprox)
      }
      p -> (toLiveSet(in), toLiveSet(out))
    })
    .toMap

  val inout = readWrites.collect {
    case (proc, rws) if p.mainProcedure == proc => {
      // no callers of main procedure so keep the whole read/write set
      // of registers
      proc -> ((lives(proc)._1.intersect(rws.reads)), (overapprox.intersect(rws.writes)))
    }
    case (proc, rws) => {
      val liveStart = lives(proc)._1
      val liveEnd = lives(proc)._2

      proc -> (liveStart.intersect(rws.reads), liveEnd.intersect(rws.writes))
    }
  }.toMap

  inout.withDefaultValue((overapprox, overapprox))
}

class SetActualParams(
    val inBinding: Map[Procedure, Map[LocalVar, Variable]],
    val outBinding: Map[Procedure, Map[LocalVar, Variable]],
    val externalFunctions: Set[String]
) extends CILVisitor {
  // expects programs to be in single return form
  var currStmt: Option[Statement] = None

  def externalIn: Map[LocalVar, Variable] = {
    (0 to 31).map(i => LocalVar(s"R${i}_in", BitVecType(64)) -> LocalVar(s"R$i", BitVecType(64))).toMap
  }
  def externalOut: Map[LocalVar, Variable] = {
    (0 to 31).map(i => LocalVar(s"R${i}_out", BitVecType(64)) -> LocalVar(s"R$i", BitVecType(64))).toMap
  }

  override def vproc(p: Procedure) = {
    val incoming =
      p.formalInParam.toList.flatMap(param => inBinding.get(p).flatMap(_.get(param)).map(p => Assign(p, param)).toList)
    p.entryBlock.foreach(b => b.statements.prependAll(incoming))
    DoChildren()
  }

  override def vstmt(s: Statement) = {
    currStmt = Some(s)
    s match {
      // case d: DirectCall if !externalFunctions.contains(d.target.name) => {
      //   // we have changed the parameter-passed variable to locals so we have LocalVar(n) -> LocalVar(n)
      //   for (binding <- inBinding.get(d.target)) {
      //     if (externalFunctions.contains(d.target.name)) {
      //       d.actualParams = SortedMap.from(binding)
      //     } else {
      //       d.actualParams = d.actualParams ++ SortedMap.from(binding)
      //     }
      //   }
      //   for (binding <- outBinding.get(d.target)) {
      //     d.outParams = SortedMap.from(binding)
      //   }
      // }
      case d: DirectCall => {
        d.actualParams = SortedMap.from(d.target.inParamDefaultBinding)
        d.outParams = SortedMap.from(d.target.outParamDefaultBinding)
      }
      case _ => ()
    }
    DoChildren()
  }

  override def vjump(j: Jump) = {
    j match {
      case r: Return => {
        r.outParams = SortedMap.from(r.parent.parent.outParamDefaultBinding)
        DoChildren()
      }
      case _ => DoChildren()
    }
  }

  override def vlvar(v: Variable) = {
    ChangeTo(LocalVar(v.name, v.getType))
  }
  override def vrvar(v: Variable) = {
    ChangeTo(LocalVar(v.name, v.getType))
  }

}

def specToProcForm(
    spec: Specification,
    mappingInparam: Map[Procedure, Map[LocalVar, Variable]],
    mappingOutparam: Map[Procedure, Map[LocalVar, Variable]]
): Specification = {
  import boogie.*

  def toNameMapping(v: Map[LocalVar, Variable]): Map[String, String] = {
    v.map(v => (v._2.name, v._1.name)) ++ v.map(v => ("Gamma_" + v._2.name, "Gamma_" + v._1.name))
  }
  val varToInVar: Map[String, Map[String, String]] = mappingInparam.map(p => (p._1.name -> toNameMapping(p._2)))
  val varToOutVar: Map[String, Map[String, String]] = mappingOutparam.map(p => (p._1.name -> toNameMapping(p._2)))

  def convVarToOld(varInPre: Map[String, String], varInPost: Map[String, String], isPost: Boolean = false)(
      b: BExpr
  ): BExpr = {
    val varToOld = convVarToOld(varInPre, varInPost, isPost)
    b match {
      case b: BVariable if isPost && varInPost.contains(b.name) => BVariable(varInPost(b.name), b.getType, b.scope)
      case b: BVariable if !isPost && varInPre.contains(b.name) => BVariable(varInPre(b.name), b.getType, b.scope)
      case b: BVar                                              => b
      // case b : _ => varToOld(b)
      case b: BLiteral       => b
      case b: BVExtract      => b.copy(body = varToOld(b.body))
      case b: BVRepeat       => b.copy(body = varToOld(b.body))
      case b: BVZeroExtend   => b.copy(body = varToOld(b.body))
      case b: BVSignExtend   => b.copy(body = varToOld(b.body))
      case b: BFunctionCall  => b.copy(args = b.args.map(varToOld))
      case b: UnaryBExpr     => b.copy(arg = varToOld(b.arg))
      case b: BinaryBExpr    => b.copy(arg1 = varToOld(b.arg1), arg2 = varToOld(b.arg2))
      case b: IfThenElse     => IfThenElse(varToOld(b.guard), varToOld(b.thenExpr), varToOld(b.elseExpr))
      case b: QuantifierExpr => b
      case b: Old => {
        if (isPost) {
          convVarToOld(varInPre, varInPost, false)(b.body)
        } else {
          throw Exception("Illegal nested or non-relation Old()")
        }
      }
      case b: MapAccess    => b.copy(index = varToOld(b.index))
      case b: MapUpdate    => b.copy(index = varToOld(b.index), value = varToOld(b.value))
      case b: BByteExtract => b.copy(value = varToOld(b.value), offset = varToOld(b.offset))
      case b: BInBounds    => b.copy(base = varToOld(b.base), len = varToOld(b.len), i = varToOld(b.i))
      case b: BMemoryLoad  => b.copy(index = varToOld(b.index))
      case b: BMemoryStore => b.copy(index = varToOld(b.index), value = varToOld(b.value))
      case b: BDirectExpr  => b
      case b: GammaLoad    => b.copy(index = varToOld(b.index))
      case b: GammaStore   => b.copy(index = varToOld(b.index), value = varToOld(b.value))
      case b: L            => b.copy(index = varToOld(b.index))
      case b: SpecVar      => b
    }
  }

  val ns = spec.copy(subroutines = spec.subroutines.map(s => {
    s.copy(
      requires = s.requires.map(convVarToOld(varToInVar(s.name), varToOutVar(s.name), false)),
      ensures = s.ensures.map(convVarToOld(varToInVar(s.name), varToOutVar(s.name), true))
    )
  }))
  ns
}
