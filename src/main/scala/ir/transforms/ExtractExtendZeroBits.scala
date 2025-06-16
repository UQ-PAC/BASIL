package ir.transforms

import ir.transforms.interprocSummaryFixpointSolver
import analysis.*
import ir.*
import ir.cilvisitor.*
import collection.immutable.SortedMap

object ExtractExtendZeroBits {

  def resultToTransform(result: Map[Variable, TNum]): Map[Variable, Variable] = {
    val d = TNumDomain()

    def bitSet(b: BitVecLiteral, n: Int) = {
      ((b.value >> n) & 1) == 1
    }

    def highBitsZero(n: TNum): Option[Int] = {
      var num: Option[Int] = None

      val maxSet = n.mayBits

      var delt = false
      for (r <- (maxSet.size - 1) to 0 by -1) {
        if (bitSet(maxSet, r)) {
          delt = true
        } else {
          num = Some(r)
        }
      }
      num match {
        case Some(n) if n < maxSet.size => Some(n)
        case _ => None
      }
    }

    val varReplace = result.flatMap {
      case (v: Variable, tn: TNum) => {
        highBitsZero(tn) match {
          case None => None
          case Some(mostSigZeroBit) => {
            v match {
              case l @ LocalVar(n, BitVecType(sz), i) =>
                Some(v -> l.copy(irType = BitVecType(sz - mostSigZeroBit)))
              case l @ Register(n, sz) =>
                Some(v -> l.copy(size = sz - mostSigZeroBit))
              case _ => None
            }
          }
        }
      }
    }
    varReplace
  }

  class ReplaceSigns(procReplacements: Map[Procedure, Map[Variable, Variable]]) extends CILVisitor {

    var replacement: Map[Variable, Variable] = Map()

    override def vproc(proc: Procedure) = {
      replacement = procReplacements.get(proc).getOrElse(Map())

      // change formal param function signature
      val newIn = proc.formalInParam.foreach {
        case p if replacement.contains(p) => {
          proc.formalInParam.remove(p)
          proc.formalInParam.add(replacement(p).asInstanceOf[LocalVar])
        }
        case _ => ()
      }
      val newOut = proc.formalOutParam.foreach {
        case p if replacement.contains(p) => {
          proc.formalOutParam.remove(p)
          proc.formalOutParam.add(replacement(p).asInstanceOf[LocalVar])
        }
        case _ => ()
      }

      // fixup calls to this function for new signature
      for (call <- proc.incomingCalls()) {
        call.outParams = SortedMap.from(call.outParams.map {
          case (p, v) if replacement.contains(p) && replacement.contains(v) =>
            val l = replacement.getOrElse(p, p).asInstanceOf[LocalVar]
            val r = replacement.getOrElse(v, v)
            l -> r
          case p => p
        })

        call.actualParams = SortedMap.from(call.actualParams.map {
          case (p, e0) if replacement.contains(p) => {
            val e = visit_expr(this, e0)
            val l = replacement.getOrElse(p, p).asInstanceOf[LocalVar]
            val r = if size(e).get != size(l).get then Extract(size(l).get, 0, e) else e
            l -> r
          }
          case p => p
        })

      }
      DoChildren()
    }

    override def vjump(j: Jump) = j match {
      case r: Return => {
        ChangeDoChildrenPost(
          r,
          _ match {
            case r: Return => {
              r.outParams = SortedMap.from(r.outParams.map {
                case (v, e) if replacement.contains(v) =>
                  val repl: LocalVar = replacement(v).asInstanceOf[LocalVar]
                  val rhs = if size(e).get != size(repl).get then Extract(size(repl).get, 0, e) else e
                  repl -> rhs
                case o => o
              })
              r
            }
            case o => o
          }
        )
      }
      case _ => DoChildren()
    }

    override def vstmt(s: Statement) = {
      s match {
        case l @ LocalAssign(lhs, rhs, _) if replacement.contains(lhs) => {
          l.lhs = replacement(lhs)
          l.rhs = Extract(size(replacement(lhs)).get, 0, rhs)
        }
        case l @ MemoryAssign(lhs, rhs, _) if replacement.contains(lhs) => {
          l.lhs = replacement(lhs)
          l.rhs = Extract(size(replacement(lhs)).get, 0, rhs)
        }
        case m: MemoryLoad => {
          // not possible to replace lhs
          ()
        }
        // case s: SimulAssign => {
        //  s.assignments = s.assignments.map {
        //    case (lhs, rhs) if replacement.contains(lhs) => {
        //      replacement(lhs) -> Extract(size(replacement(lhs)).get, 0, rhs)
        //    }
        //    case o => o
        //  }
        // }
        case _ => ()
      }
      DoChildren()
    }

    override def vexpr(e: Expr) = e match {
      case v: Variable if replacement.contains(v) => {
        val r = replacement(v)
        ChangeTo(ZeroExtend(size(v).get - size(r).get, r))
      }
      case _ => DoChildren()
    }
  }

  def doTransform(p: Program) = {
    val d = TNumDomain()

    class SummaryGen extends ProcedureSummaryGenerator[Map[Variable, TNum], Map[Variable, TNum]] {

      val dom = TNumDomain()
      override def bot = Map[Variable, TNum]()
      override def top = Map[Variable, TNum]()
      override def join(l: Map[Variable, TNum], r: Map[Variable, TNum], pos: Procedure) = dom.join(l, r)
      // overrided in analysis
      override def transfer(l: Map[Variable, TNum], b: Procedure) = ???

      def localTransferCall(
        l: Map[Variable, TNum],
        summaryForTarget: Map[Variable, TNum],
        p: DirectCall
      ): Map[Variable, TNum] = {
        val joined = p.outParams.map {
          case (formal, lhs) => {
            val joined = (summaryForTarget.get(formal), l.get(lhs)) match {
              case (Some(a), Some(b)) => a.join(b)
              case (Some(a), None) => a
              case (None, Some(a)) => a
              case (None, None) => TNum.top(d.sizeBits(formal.irType))
            }
            lhs -> joined
          }
        }

        l ++ joined.toMap
      }
      def updateSummary(
        prevSummary: Map[Variable, TNum],
        p: Procedure,
        resBefore: Map[Block, Map[Variable, TNum]],
        resAfter: Map[Block, Map[Variable, TNum]]
      ): Map[Variable, TNum] = {
        p.returnBlock.flatMap(resAfter.get(_)).getOrElse(Map())
      }
    }
    applyRPO(p)

    val solver = interprocSummaryFixpointSolver(d, SummaryGen())

    val result = solver.solveProgInterProc(p)

    val tx = p.procedures
      .filter(result.contains)
      .map(proc => {
        val r = result(proc)
        val tx = resultToTransform(r)
        proc -> tx
      })
      .toMap

    val vis = ReplaceSigns(tx)
    visit_prog(vis, p)

    visit_prog(ir.eval.SimpExpr(ir.eval.simplifyPaddingAndSlicingExprFixpoint), p)

    assert(invariant.correctCalls(p))
  }

}
