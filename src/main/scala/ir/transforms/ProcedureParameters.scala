package ir.transforms
import ir.cilvisitor.*
import ir.*
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
import collection.immutable.SortedMap
import specification.Specification

/** Use ANR and RNA to identify procedure parameters.
  */

// procedure ->
//  rna at beginning = in params
//  anr at return = out parms

// call : forall vars in scope if name matches formal inparams of target = actual parameters
// return : forall vars in scope if name matches formal outparams of proc = actual outparams

def liftProcedureCallAbstraction(ctx: util.IRContext) : util.IRContext = {
  val p = ctx.program
  val anr = analysis.ANRAnalysisSolver(p, false).analyze()
  val rna = analysis.RNAAnalysisSolver(p, false).analyze()

  // functions for which we don't know their behaviour and assume they modify all registers
  val external = ctx.externalFunctions.map(_.name) ++ ctx.program.collect {
    case b: Procedure if b.blocks.isEmpty => b.name
  }

  val formalParams = SetFormalParams(anr, rna, external)
  visit_prog(formalParams, p)
  val actualParams = SetActualParams(formalParams.mappingInparam, formalParams.mappingOutparam, external)
  visit_prog(actualParams, p)

  ctx.copy(specification = specToProcForm(ctx.specification, formalParams.mappingInparam, formalParams.mappingOutparam))
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

class SetFormalParams(val anr: Map[CFGPosition, Set[Variable]], val rna: Map[CFGPosition, Set[Variable]],
    val externalFunctions: Set[String]
  )
    extends CILVisitor {
  // expects programs to be in single return form

  var mappingOutparam = Map[Procedure, Map[LocalVar, Variable]]()
  var mappingInparam = Map[Procedure, Map[LocalVar, Variable]]()

  def externalIn : Map[LocalVar, Variable] = {
    (0 to 31).map(i => LocalVar(s"R${i}_in", BitVecType(64)) -> LocalVar(s"R$i", BitVecType(64))).toMap
  }
  def externalOut : Map[LocalVar, Variable] = {
    (0 to 31).map(i => LocalVar(s"R${i}_out", BitVecType(64)) -> LocalVar(s"R$i", BitVecType(64))).toMap
  }

  override def vproc(p: Procedure) = {
    if (externalFunctions.contains(p.name)) {
      p.formalInParam = mutable.SortedSet.from(externalIn.map(_._1))
      p.formalOutParam = mutable.SortedSet.from(externalOut.map(_._1))
      mappingInparam = mappingInparam.updated(p, externalIn)
      mappingOutparam = mappingOutparam.updated(p, externalOut)
      SkipChildren()
    } else {
      val (lvars, rvars) = collectVariables(p)

      val in = IRWalk.firstInProc(p)
      if (in.isDefined) {
        val inparams = (rna(in.get).toSet -- p.formalInParam).map(v =>
          (LocalVar(v.name + "_in", v.getType), LocalVar(v.name, v.getType))
        )
        p.formalInParam.addAll(inparams.map(_._1))
        mappingInparam = mappingInparam.updated(p, inparams.toMap)
      }

      // outparams is everything touched
      val outparams = lvars.map(v => LocalVar(v.name + "_out", v.getType) -> LocalVar(v.name, v.getType))
      p.formalOutParam = mutable.SortedSet.from(outparams.map(_._1))
      mappingOutparam = mappingOutparam.updated(p, outparams.toMap)

      SkipChildren()
    }
  }
}

class SetActualParams(
    val inBinding: Map[Procedure, Map[LocalVar, Variable]],
    val outBinding: Map[Procedure, Map[LocalVar, Variable]],
    val externalFunctions: Set[String]
) extends CILVisitor {
  // expects programs to be in single return form
  var currStmt: Option[Statement] = None

  def externalIn : Map[LocalVar, Variable] = {
    (0 to 31).map(i => LocalVar(s"R${i}_in", BitVecType(64)) -> LocalVar(s"R$i", BitVecType(64))).toMap
  }
  def externalOut : Map[LocalVar, Variable] = {
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
      case d: DirectCall if !externalFunctions.contains(d.target.name) => {
        // we have changed the parameter-passed variable to locals so we have LocalVar(n) -> LocalVar(n)
        for (binding <- inBinding.get(d.target)) {
          if (externalFunctions.contains(d.target.name)) {
            d.actualParams = SortedMap.from(binding)
          } else {
            d.actualParams = d.actualParams ++ SortedMap.from(binding)
          }
        }
        for (binding <- outBinding.get(d.target)) {
          d.outParams = SortedMap.from(binding)
        }
      }
      case d: DirectCall /* if external */ => {
          d.actualParams = SortedMap.from(externalIn)
          d.outParams = SortedMap.from(externalOut)
      }
      case _ => ()
    }
    DoChildren()
  }

  override def vjump(j: Jump) = {
    j match {
      case r: Return => {
        for (binding <- outBinding.get(r.parent.parent)) {
          r.outParams = SortedMap.from(binding)
        }
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

  def toNameMapping(v : Map[LocalVar, Variable]): Map[String, String] = {
    v.map(v => (v._2.name, v._1.name)) ++ v.map(v => ("Gamma_" + v._2.name, "Gamma_" + v._1.name))
  }
  val varToInVar: Map[String, Map[String, String]] = mappingInparam.map(p => (p._1.name -> toNameMapping(p._2)))
  val varToOutVar: Map[String, Map[String, String]]  = mappingOutparam.map(p => (p._1.name -> toNameMapping(p._2)))

  def convVarToOld(varInPre: Map[String, String], varInPost: Map[String, String], isPost: Boolean = false)(b: BExpr): BExpr = {
    val varToOld = convVarToOld(varInPre, varInPost, isPost)
    b match {
      case b: BVariable if isPost && varInPost.contains(b.name) => BVariable(varInPost(b.name), b.getType, b.scope)
      case b: BVariable if !isPost && varInPre.contains(b.name) => BVariable(varInPre(b.name), b.getType, b.scope)
      case b: BVar => b
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
      case b: Old            => {
        if (isPost) {
          convVarToOld(varInPre, varInPost, false)(b.body)
        } else {
          throw Exception("Illegal nested or non-relation Old()")
        }
      }
      case b: MapAccess      => b.copy(index = varToOld(b.index))
      case b: MapUpdate      => b.copy(index = varToOld(b.index), value = varToOld(b.value))
      case b: BByteExtract   => b.copy(value = varToOld(b.value), offset = varToOld(b.offset))
      case b: BInBounds      => b.copy(base = varToOld(b.base), len = varToOld(b.len), i = varToOld(b.i))
      case b: BMemoryLoad    => b.copy(index = varToOld(b.index))
      case b: BMemoryStore   => b.copy(index = varToOld(b.index), value = varToOld(b.value))
      case b: BDirectExpr    => b
      case b: GammaLoad      => b.copy(index = varToOld(b.index))
      case b: GammaStore     => b.copy(index = varToOld(b.index), value = varToOld(b.value))
      case b: L              => b.copy(index = varToOld(b.index))
      case b: SpecVar        => b
    }
  }


  println(spec.subroutines)
  val ns = spec.copy(subroutines = spec.subroutines.map(s => {
    s.copy(requires = 
      s.requires.map(convVarToOld(varToInVar(s.name), varToOutVar(s.name), false)), 
      ensures = s.ensures.map(convVarToOld(varToInVar(s.name), varToOutVar(s.name), true)))
  }))
  println(ns.subroutines)
  ns
}

