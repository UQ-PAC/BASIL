package ir.transforms

import ir.*
import ir.cilvisitor.*
import util.tvEvalLogger

import scala.util.chaining.scalaUtilChainingOps

object CountGuardStatements {

  enum GuardComplexity {
    case HasFlagRegisters(regs: Set[GlobalVar])
    case OpsTooMany(ops: List[BinOp | UnOp])
    case OpsTooComplex(ops: List[BoolToBV1.type])
    case VarsTooMany(vars: Set[Variable])
  }

  def listOpsInExpr(e: Expr): List[BinOp | UnOp] = e match {
    case AssocExpr(op, es) => op +: es.flatMap(listOpsInExpr)
    case BinaryExpr(op, x, y) => op +: listOpsInExpr(x) ++: listOpsInExpr(y)
    case UnaryExpr(op, x) => op +: listOpsInExpr(x)
    case _ => List()
  }

  def classifyGuard(s: Assume): List[GuardComplexity] = {
    val vars = s.body.variables

    val flags = vars.collect { case reg @ Register(_, 1) =>
      reg
    }

    val ops = listOpsInExpr(s.body)

    val complexOps = ops.collect { case x @ BoolToBV1 =>
      BoolToBV1
    }

    (Option.when(flags.nonEmpty) { GuardComplexity.HasFlagRegisters(flags) }

    // allowable number of operations is `2 * vars + 1` to allow for
    // one possibly-negated binary operation per variable. plus one top-level
    // operation like ==.
      ++ Option.when(ops.size > 2 * vars.size + 1) { GuardComplexity.OpsTooMany(ops) }

      ++ Option.when(complexOps.nonEmpty) { GuardComplexity.OpsTooComplex(complexOps) }

      ++ Option.when(vars.size > 2) { GuardComplexity.VarsTooMany(vars) }).toList
  }
}

class CountGuardStatements extends CILVisitor {
  import CountGuardStatements.*

  var guards: List[(Assume, List[GuardComplexity])] = Nil

  override def vstmt(s: Statement) = {
    s match {
      case ass: Assume if ass.checkSecurity =>
        guards = (ass -> classifyGuard(ass)) +: guards
      case _ => ()
    }
    SkipChildren()
  }

  def reportToLog(label: String, log: String => Unit = tvEvalLogger.debug(_)) = {
    val entries = guards.flatMap { (k, vs) => vs.map(k -> _) }
    val groupedByViolation = entries.groupMapReduce(_._2.productPrefix)(_._1.pipe(List(_)))(_ ++ _)

    log(s"tv-eval-marker: $label-guard-total-count=" + guards.size)
    log(s"tv-eval-marker: $label-guard-simple-count=" + guards.count(_._2.isEmpty))
    log(s"tv-eval-marker: $label-guard-complex-unique-count=" + guards.count(_._2.nonEmpty))

    groupedByViolation.foreach { (violation, guards) =>
      log(s"tv-eval-marker: $label-guard-complex-$violation-count=" + guards.size)
    }

    if (label == "after") {
      println(guards.filter(_._2.nonEmpty))
    }
  }
}
