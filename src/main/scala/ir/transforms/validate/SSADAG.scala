package ir.transforms.validate

import ir.*
import ir.transforms.Substitute
import util.tvLogger

import scala.collection.mutable

import cilvisitor.*

object SSADAG {

  /**
  * Convert an acyclic CFA to a transition encoding
  *
  * Returns the SSA renaming for each block entry in the CFA.
  */
  def transform(
    frames: Map[String, CallParamMapping],
    p: Procedure,
    inputs: List[Variable],
    outputs: List[Variable],
    liveVarsBefore: Map[String, Set[Variable]]
  ) = {
    convertToMonadicSideEffect(frames, p)

    ssaTransform(p, inputs, outputs, liveVarsBefore)
  }

  private class Passify extends CILVisitor {
    override def vstmt(s: Statement) = s match {
      case l @ SideEffectStatement(s, n, lhs, rhs) => {
        // assume ackermann
        // FIXME: should probably go in the ackermann pass?
        ChangeTo(List())
      }
      case SimulAssign(assignments, _) => {
        ChangeTo(List(Assume(boolAnd(assignments.map(polyEqual)))))
      }
      case _ => SkipChildren()
    }
  }

  def passify(p: Procedure) = {
    visit_proc(Passify(), p)
  }

  def convertToMonadicSideEffect(frames: Map[String, CallParamMapping], p: Procedure) = {

    class MonadicConverter(frames: Map[String, CallParamMapping]) extends CILVisitor {
      val SF = SideEffectStatementOfStatement(frames)
      override def vstmt(s: Statement) = s match {
        case SF(s) => ChangeTo(List(s))
        case _ => SkipChildren()
      }
    }

    visit_proc(MonadicConverter(frames), p)

  }

  def blockDoneVar(b: Block) = {
    LocalVar(b.label + "_done", BoolType)
  }

  /**
  * Convert an acyclic CFA to a transition encoding
  *
  * Returns the SSA renaming for each block entry in the CFA.
  */
  def ssaTransform(
    p: Procedure,
    inputs: List[Variable],
    outputs: List[Variable],
    liveVarsBefore: Map[String, Set[Variable]]
  ): (((String, Expr) => Expr), Map[BlockID, Map[Variable, Variable]]) = {

    var renameCount = 0
    val stRename = mutable.Map[Block, Map[Variable, Variable]]()
    val renameBefore = mutable.Map[Block, Map[Variable, Variable]]()

    def blockDone(b: Block) = blockDoneVar(b)

    var count = Map[String, Int]()

    def freshName(v: Variable) =
      renameCount = count.get(v.name).getOrElse(0) + 1
      count = count + (v.name -> renameCount)
      v match {
        case l: LocalVar => l.copy(varName = l.name + "_AT" + renameCount, index = 0)
        case l: GlobalVar => l.copy(name = l.name + "_AT" + renameCount)
      }

    for (eb <- p.entryBlock) {
      // renameBefore(eb) = inputs.map(i => i -> freshName(i)).toMap
    }

    class RenameRHS(rn: Variable => Option[Variable]) extends CILVisitor {
      override def vrvar(v: Variable) = ChangeTo(rn(v).getOrElse(v))
      override def vexpr(e: Expr) = {
        ChangeTo(Substitute(rn, false)(e).getOrElse(e))
      }
      override def vstmt(s: Statement) = s match {
        case se @ SideEffectStatement(s, n, lhs, rhs) =>
          se.rhs = rhs.map((f, e) => (f, visit_expr(this, e)))
          SkipChildren()
        case _ => DoChildren()

      }
    }

    def renameRHS(rename: Variable => Option[Variable])(c: Command): Command = c match {
      // rename all rvars
      case s: Statement => visit_stmt(RenameRHS(rename), s).head
      case s: Jump => visit_jump(RenameRHS(rename), s)
    }

    ir.transforms.reversePostOrder(p)
    val worklist = mutable.PriorityQueue[Block]()(Ordering.by(_.rpoOrder))
    worklist.addAll(p.blocks)

    class RenameLHS(subst: Variable => Option[Variable]) extends CILVisitor {

      override def vstmt(s: Statement) = s match {
        case s @ SideEffectStatement(_, _, lhs, _) =>
          s.lhs = lhs.map((f, e) => (f, visit_lvar(this, e)))
          SkipChildren()
        case _ => DoChildren()

      }

      override def vlvar(v: Variable) = subst(v) match {
        case Some(vn) => ChangeTo(vn)
        case none => SkipChildren()
      }
    }

    def renameLHS(substs: Map[Variable, Variable], s: Statement) = s match {
      case s: Statement => visit_stmt(RenameLHS(substs.get), s)
    }

    val phiLabel = "TVSSADAGPHI"

    var phis = 0
    while (worklist.nonEmpty) {
      val b = worklist.dequeue()

      var blockDoneCond = List[Expr](boolOr(b.prevBlocks.map(blockDone).toList))

      def live(v: Variable) =
        liveVarsBefore.get(b.label).forall(_.contains(v)) || v.name.startsWith("SYNTH") || v.name.startsWith(
          "TRACE"
        ) || outputs.contains(v) || inputs.contains(v)

      if (b.prevBlocks.nonEmpty) then {
        // val defines: Iterable[(Block, Seq[(Variable, Variable)])] =
        //  b.prevBlocks.flatMap(b => stRename.get(b).map(renames => b -> renames.toSeq.filter((k, v) => live(k))))
        // var varToRenamings: Map[Variable, Iterable[(Block, Variable, Variable)]] =
        //  defines
        //    .flatMap { case (b, rns) =>
        //      rns.map { case (v, rn) =>
        //        (b, v, rn)
        //      }
        //    }
        //    .groupBy(_._2)

        val defines: Seq[Variable] =
          (b.prevBlocks.toSeq.flatMap(b => stRename.get(b).toSeq.flatMap(_.map(_._1).filter(live)))).toSet.toSeq

        var nrenaming = mutable.Map.from(Map[Variable, Variable]())

        defines.foreach((v: Variable) => {
          val defsToJoin = b.prevBlocks.map(b => b -> stRename.get(b).flatMap(_.get(v)).getOrElse(v))
          val inter = defsToJoin.map(_._2).toSet
          if (inter.size == 1) {
            nrenaming(v) = inter.head
          } else {

            val fresh = freshName(v)

            val grouped = defsToJoin.groupBy(_._2).map {
              case (ivar, blockset) => {
                val blocks = blockset.map(_._1)
                BinaryExpr(BoolIMPLIES, boolOr(blocks.map(blockDone)), polyEqual(ivar, fresh))
              }
            }

            val phicond = grouped.toList

            // val phicond = defsToJoin.map((b, nv) => {
            //  BinaryExpr(BoolIMPLIES, blockDone(b), polyEqual(nv, fresh))
            // }).toList

            val phiscond = if (phicond.length > 8) then {
              phicond.map(b => Assume(b, None, Some(phiLabel)))
            } else Seq(Assume(boolAnd(phicond), None, Some(phiLabel)))

            phis += phicond.length
            b.statements.prependAll(phiscond)

            nrenaming(v) = fresh
          }
        })

        stRename(b) = nrenaming.toMap
      } else {
        val rn = mutable.Map.from(renameBefore.getOrElse(b, Map()))
        stRename(b) = rn.toMap
        rn
      }

      if (!b.parent.entryBlock.contains(b)) {
        renameBefore(b) = stRename.getOrElse(b, Map())
      }

      val renaming = mutable.Map.from(renameBefore.getOrElse(b, Map()))

      def isPhi(s: Statement) = s match {
        case a: Assume if a.label.contains(phiLabel) => true
        case _ => false
      }

      for (s <- b.statements.toList.filterNot(isPhi)) {
        val c = renameRHS(renaming.get)(s) // also modifies in-place

        c match {
          case a @ Assume(cond, _, _, _) =>
            blockDoneCond = cond :: blockDoneCond
          case _ => ()
        }

        c match {
          case a @ SideEffectStatement(s, n, lhs, rhs) => {
            // note this matches some assume statements
            // where checkSecurity = true
            val rn = lhs
              .map((formal, v) => {
                val freshDef = freshName(v)
                renaming(v) = freshDef
                v -> freshDef
              })
              .toMap

            renameLHS(rn, a)
          }
          case a @ Assume(cond, _, _, _) =>
            b.statements.remove(a)
          case a: Assign => {
            a.assignees.foreach(v => {
              val freshDef = freshName(v)
              renameLHS(Map(v -> freshDef), a)
              renaming(v) = freshDef
            })
          }
          case _ => ()
        }
      }

      renameRHS(renaming.get)(b.jump)
      stRename(b) = renaming.toMap

      val c =
        if (b.parent.entryBlock.contains(b) || b.label.endsWith("SYNTH_ENTRY")) then TrueLiteral
        else boolAnd(blockDoneCond)

      b.statements.append(LocalAssign(blockDone(b), c))
      if (b.label.contains("SYNTH_EXIT")) {
        b.statements.append(Assert(blockDone(b), Some("blockdone")))
      }
    }

    tvLogger.debug("Phi node count: " + phis)
    val renameBeforeLabels = renameBefore.map((b, r) => b.label -> r)

    ((b, c) => visit_expr(RenameRHS(renameBeforeLabels(b).get), c), renameBeforeLabels.toMap)
  }
}
