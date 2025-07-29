package ir.transforms.validate

import ir.*
import ir.transforms.Substitute

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
  ): ((String, Expr) => Expr) = {

    var renameCount = 0
    val stRename = mutable.Map[Block, mutable.Map[Variable, Variable]]()
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
      renameBefore(eb) = inputs.map(i => i -> freshName(i)).toMap
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

    while (worklist.nonEmpty) {
      val b = worklist.dequeue()
      var blockDoneCond = List[Expr](boolOr(b.prevBlocks.map(blockDone).toList))

      var phis = Vector[Statement]()

      def live(v: Variable) = 
        v.name.startsWith("SYNTH") || v.name.startsWith("TRACE") || outputs.contains(v) || inputs.contains(v) || liveVarsBefore.get(b.label).forall(_.contains(v))

      var renaming = if (b.prevBlocks.nonEmpty) then {
        var joinedRenames = Map[Variable, Variable]()
        val defines: Iterable[(Block, Seq[(Variable, Variable)])] =
          b.prevBlocks.flatMap(b => stRename.get(b).map(renames => b -> renames.toSeq.filter((k, v) => live(k))))
        var varToRenamings: Map[Variable, Iterable[(Block, Variable, Variable)]] =
          defines
            .flatMap { case (b, rns) =>
              rns.map { case (v, rn) =>
                (b, v, rn)
              }
            }
            .groupBy(_._2)
        var inter: Set[Variable] = varToRenamings.collect {
          case (v, defset) if defset.map(_._3).toSet.size > 1 => v
        }.toSet
        var disjoint: Map[Variable, Variable] = varToRenamings.collect {
          case (v, defset) if !inter.contains(v) => {
            assert(defset.tail.forall(_._3 == defset.head._3))
            (defset.head._2, defset.head._3)
          }
        }.toMap
        var nrenaming = mutable.Map.from(disjoint)
        inter.foreach(v => {

          val defsToJoin =
            b.prevBlocks.filter(b => stRename.get(b).exists(_.contains(v)))

          val fresh = freshName(v)

          val oneOf = boolOr(defsToJoin.map(blockDone))
          val phicond = defsToJoin.map(b => {
            BinaryExpr(BoolIMPLIES, blockDone(b), polyEqual(stRename(b)(v), fresh))
          })

          val phiscond = if (phicond.toList.length > 8) then {
            phicond.map(b => Assume(b))
          } else Seq(Assume(boolAnd(phicond)))
          phis = phis ++ phiscond

          joinedRenames = joinedRenames + (v -> fresh)
        })

        nrenaming ++= joinedRenames

        stRename(b) = nrenaming
        b.statements.prependAll(phis)
        stRename(b)
      } else {
        val rn = mutable.Map.from(renameBefore.getOrElse(b, Map()))
        rn

      }
      if (!b.parent.entryBlock.contains(b)) {
        renameBefore(b) = stRename.getOrElse(b, mutable.Map()).toMap
      }

      for (s <- b.statements.toList) {
        val c = renameRHS(renaming.get)(s) // also modifies in-place
        c match {
          case a @ SideEffectStatement(s, n, lhs, rhs) => {
            val rn = lhs
              .map((formal, v) => {
                val freshDef = freshName(v)
                renaming(v) = freshDef
                v -> freshDef
              })
              .toMap

            renameLHS(rn, a)
          }
          case a @ Assume(cond, _, _, _) if !phis.contains(a) =>
            blockDoneCond = cond :: blockDoneCond
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
      stRename(b) = renaming

      val c =
        if (b.parent.entryBlock.contains(b) || b.label.endsWith("SYNTH_ENTRY")) then TrueLiteral
        else boolAnd(blockDoneCond)

      b.statements.append(LocalAssign(blockDone(b), c))
      if (b.label.contains("SYNTH_EXIT")) {
        b.statements.append(Assert(blockDone(b), Some("blockdone")))
      }
    }

    val renameBeforeLabels = renameBefore.map((b, r) => b.label -> r)

    (b, c) => visit_expr(RenameRHS(renameBeforeLabels(b).get), c)
  }
}
