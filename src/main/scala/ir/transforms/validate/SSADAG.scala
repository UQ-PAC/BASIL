package ir.transforms.validate

import scala.collection.mutable
import ir.*
import cilvisitor.*
import ir.transforms.Substitute
import analysis.ProcFrames.*

object SSADAG {

  /**
  * Convert an acyclic CFA to a transition encoding
  *
  * Returns the SSA renaming for each block entry in the CFA.
  */
  def transform(p: Procedure) = {
    ssaTransform(p)
  }

  class Passify extends CILVisitor {
    override def vstmt(s: Statement) = s match {
      case SimulAssign(assignments, _) => {
        ChangeTo(List(Assume(boolAnd(assignments.map(polyEqual)))))
      }
      case _ => SkipChildren()
    }
  }

  def passify(p: Procedure) = {
    visit_proc(Passify(), p)
  }

  /**
  * Convert an acyclic CFA to a transition encoding
  *
  * Returns the SSA renaming for each block entry in the CFA.
  */
  def ssaTransform(p: Procedure): ((Block, Expr) => Expr) = {
    // FIXME:  apply transform to global variables

    var renameCount = 0
    val stRename = mutable.Map[Block, mutable.Map[Variable, Variable]]()
    val renameBefore = mutable.Map[Block, Map[Variable, Variable]]()

    def blockDone(b: Block) = {
      LocalVar(b.label + "_done", BoolType)
    }

    var count = Map[String, Int]()

    def freshName(v: Variable) =
      renameCount = count.get(v.name).getOrElse(0) + 1
      count = count + (v.name -> renameCount)
      v match {
        case l: LocalVar => l.copy(varName = l.name + "_AT" + renameCount, index = 0)
        case l: GlobalVar => l.copy(name = l.name + "_AT" + renameCount)
      }

    class Subst(rn: Variable => Option[Variable]) extends CILVisitor {
      override def vrvar(v: Variable) = ChangeTo(rn(v).getOrElse(v))
      override def vexpr(e: Expr) = {
        ChangeTo(Substitute(rn, false)(e).getOrElse(e))
      }
    }

    def renameRHS(rename: Variable => Option[Variable])(c: Command): Command = c match {
      // rename all rvars
      case s: Statement => visit_stmt(Subst(rename), s).head
      case s: Jump => visit_jump(Subst(rename), s)
    }

    ir.transforms.reversePostOrder(p)
    val worklist = mutable.PriorityQueue[Block]()(Ordering.by(_.rpoOrder))
    worklist.addAll(p.blocks)

    class RenameLHS(subst: Variable => Option[Variable]) extends CILVisitor {
      override def vlvar(v: Variable) = subst(v) match {
        case Some(vn) => ChangeTo(vn)
        case none => SkipChildren()
      }
    }

    def renameLHS(substs: Map[Variable, Variable], s: Statement) = {
      visit_stmt(RenameLHS(substs.get), s)
    }

    while (worklist.nonEmpty) {
      val b = worklist.dequeue()
      var blockDoneCond = List[Expr](boolOr(b.prevBlocks.map(blockDone).toList))
      // val onlyOne = boolAnd(for {
      //  l <- b.prevBlocks
      //  r <- b.prevBlocks.filterNot(_ == l)
      //  neq = UnaryExpr(BoolNOT, BinaryExpr(BoolAND, blockDone(l), blockDone(r)))
      // } yield neq)

      var phis = Vector[Statement]()

      var renaming = if (b.prevBlocks.nonEmpty) then {
        var joinedRenames = Map[Variable, Variable]()
        val defines = b.prevBlocks.flatMap(b => stRename.get(b).map(b -> _).toSeq)
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
        stRename.get(b).getOrElse(mutable.Map())
      }
      renameBefore(b) = stRename.getOrElse(b, mutable.Map()).toMap

      for (s <- b.statements.toList) {
        val c = renameRHS(renaming.get)(s) // also modifies in-place
        c match {
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

    (b, c) => visit_expr(Subst(renameBefore(b).get), c)
  }
}
