package analysis
import ir.*
import ir.cilvisitor.*

import transforms.*

object ProcFrames {

  case class Frame(
    modifiedGlobalVars: Set[GlobalVar] = Set(),
    modifiedMem: Set[Memory] = Set(),
    readGlobalVars: Set[GlobalVar] = Set(),
    readMem: Set[Memory] = Set()
  ) {
    /* coarse variable-level frame for procedures
     *
     *  - globals modified
     *  - globals captured
     *
     */

    def union(o: Frame) = {
      Frame(
        modifiedGlobalVars ++ o.modifiedGlobalVars,
        modifiedMem ++ o.modifiedMem,
        readGlobalVars ++ o.readGlobalVars,
        readMem ++ o.readMem
      )
    }
  }

  private class LocalModified(val v: Procedure => Frame, var summary: Frame) extends CILVisitor {
    override def vmem(m: Memory) = {
      summary = summary.copy(readMem = summary.readMem + m)
      SkipChildren()
    }

    override def vrvar(v: Variable) = v match {
      case v: GlobalVar =>
        summary = summary.copy(readGlobalVars = summary.readGlobalVars + v)
        SkipChildren()
      case _ =>
        SkipChildren()
    }

    override def vlvar(v: Variable) = v match {
      case v: GlobalVar =>
        summary = summary.copy(modifiedGlobalVars = summary.modifiedGlobalVars + v)
        SkipChildren()
      case _ =>
        SkipChildren()
    }

    override def vstmt(s: Statement) = {
      s match {
        case MemoryStore(m, _, _, _, _, _) => {
          summary = summary.copy(modifiedMem = summary.modifiedMem + m)
        }
        case MemoryAssign(m: GlobalVar, _, _) => {
          summary = summary.copy(modifiedGlobalVars = summary.modifiedGlobalVars + m)
        }
        case d: DirectCall => {
          summary = summary.union(v(d.target))
        }
        case _ => ()
      }
      DoChildren()
    }
  }

  private def transferProcedure(getSt: Procedure => Frame, s: Frame, p: Procedure): Frame = {
    val v = LocalModified(getSt, s)
    visit_proc(v, p)
    v.summary
  }

  def inferProcFrames(p: Program): Map[Procedure, Frame] = {
    val solver = BottomUpCallgraphWorklistSolver(transferProcedure, x => Frame(Set(), Set(), Set(), Set()))
    solver.solve(p)
  }

}
