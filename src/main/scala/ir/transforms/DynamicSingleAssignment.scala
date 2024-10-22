package ir.transforms

import util.Logger
import ir.cilvisitor.*
import ir.*
import scala.collection.mutable
import analysis._

object DynamicSingleAssignment {
  /* TODO: improve using liveness */

  def applyTransform(program: Program, liveVars: Map[CFGPosition, Set[Variable]]) = {

    case class DSARes(renames: Map[Variable, Int] = Map().withDefaultValue(-1)) // -1 means no rename

    def addIndex(v: Variable, idx: Int) = {
      if (idx != -1) {
        v match {
          case Register(n, sz) => {
            throw Exception("Should not SSA registers")
            Register(n + "_" + idx, sz)
          }
          case v @ LocalVar(n, t)  => LocalVar(v.varName, t, idx)
        }
      } else {
        v
      }
    }

    class StmtRenamer(renamesL: Map[Variable, Int] = Map(), renames: Map[Variable, Int] = Map()) extends CILVisitor {
      override def vrvar(v: Variable) = v match {
        case v if renames.contains(v) && renames(v) != -1 => ChangeTo(addIndex(v, renames(v)))
        case _                                            => DoChildren()
      }

      override def vlvar(v: Variable) = v match {
        case v if renamesL.contains(v) && renamesL(v) != -1 => ChangeTo(addIndex(v, renamesL(v)))
        case _                                              => DoChildren()
      }
    }

    def appendAssign(b: Block, s: Assign) = {
      // maintain call end of lock invariant
      if (b.statements.size > 0 && b.statements.last.isInstanceOf[Call]) {
        b.statements.insertBefore(b.statements.last, s)
      } else {
        b.statements.append(s)
      }
    }

    def njoin(st: Map[Block, DSARes], blocks: Iterable[Block] /* incoming blocks */) = {
      require(blocks.size >= 2)
      val rs = blocks.map(st(_))
      val allRenamedVars = rs.flatMap(_.renames.keySet)
      val renames = allRenamedVars.map {
        case v => {
          // for branches which have different renamings of variables, we know the larger index renaming is so far unused
          // on the branches with smaller indexes, so we add a copy to these branches renaming to the largest index
          val maxrename = rs.map(_.renames(v)).foldLeft(-1)(Integer.max)
          v -> maxrename
        }
      }.toMap

      DSARes(renames.withDefaultValue(-1))
    }

    def fixJoins(
        st: Map[Block, DSARes],
        p: Procedure,
        lhss: Map[Command, Map[Variable, Int]],
        rhss: Map[Command, Map[Variable, Int]]
    ): (Map[Command, Map[Variable, Int]], Map[Command, Map[Variable, Int]]) = {
      /**
       * Add copies to blocks who have two incoming blocks with different renamings present to unify the renamings into the 
       * subsequent block. We will expect that one of the incoming branches has a reassignment equal to this blocks entry point
       * reassignment. All other branches should have a smaller SSA index, and allow an additional copy to be added ot unify them
       * with the larger assignment.
       *
       * some consideration is required when adding copies to keep the call-end-of-block invariant
       *
       *  TODO: we can avoid adding copies for variables not live at the join
       */
      var lhs = lhss
      var rhs = rhss
      for (b <- p.blocks) {
        val incoming = b.prevBlocks
        val all = incoming.flatMap(st(_).renames.keySet)
        val outgoing = rhss(IRWalk.firstInBlock(b))

        // fix renames for all variables when when all incoming block renames rhs don't match 
        // the expected rename at this block rhs AND the variable is still live at this block 
        // by adding the appropriate copy
        for (v <- all) {
          val outgoingRename = outgoing(v)
          for (b <- incoming) {
            if (st(b).renames(v) != outgoingRename && outgoingRename != -1 && (liveVars(b).contains(v))) {
              b.statements.lastOption match {
                case Some(d: DirectCall) if d.outParams.toSet.map(_._2).contains(v) => {
                  // if there is a call on this block assigning the variable, update its outparam's ssa index
                  lhs = lhs + (d -> (lhs.get(d).getOrElse(Map()) + (v -> st(b).renames(v))))
                }
                case c => {
                  // otherwise add an assignment to the block at the end or immediately before the call, and
                  // update the ssa index of the in-parameters to the call
                  val assign = Assign(v, v, Some("appended"))
                  appendAssign(b, assign)

                  lhs = lhs + (assign -> (lhs.get(assign).getOrElse(Map()) + (v -> outgoingRename)))
                  rhs = rhs + (assign -> (rhs.get(assign).getOrElse(Map()) + (v -> st(b).renames(v))))
                  c match {
                    case Some(call: Call) => {
                      rhs = rhs + (call -> (rhs.get(call).getOrElse(Map()) + (v -> outgoingRename)))
                    }
                    case _ => ()
                  }
                }
              }
            }
          }
        }
      }
      (lhs, rhs)
    }

    def processCollect(
        i: DSARes,
        b: Block,
        count: mutable.Map[Variable, Int],
        lhs: Map[Command, Map[Variable, Int]],
        rhs: Map[Command, Map[Variable, Int]]
    ): (DSARes, Map[Command, Map[Variable, Int]], Map[Command, Map[Variable, Int]]) = {
      var r = i
      var lh = lhs
      var rh = rhs
      // push the renames through a block, incrementing the ssa index when we see a new assignment

      for (s <- b.statements) {
        rh = rh.updated(s, rh(s) ++ r.renames)
        val renames: Map[Variable, Int] = s match {
          case a: Assign => {
            if ((lh.get(a).flatMap(_.get(a.lhs))).map(_ == -1).getOrElse(true)) {
              count(a.lhs) = (count(a.lhs) + 1)
              Map(a.lhs -> count(a.lhs))
            } else {
              Map(a.lhs -> lh(a)(a.lhs))
            }
          }
          case a: DirectCall => {
            (a.outParams
              .map(_._2))
              .map(l => {
                if (lh.get(s).flatMap(_.get(l)).map(_ == -1).getOrElse(true)) {
                  count(l) = (count(l) + 1)
                  (l -> count(l))
                } else {
                  (l -> lh(s)(l))
                }
              })
              .toMap
          }
          case _ => Map()
        }
        lh = lh.updated(s, lh(s) ++ renames)
        r = r.copy(renames = r.renames ++ renames)
      }
      rh = rh.updated(b.jump, rh(b.jump) ++ r.renames)
      r = r.copy(renames = r.renames ++ rh(b.jump))

      (r, lh, rh)
    }

    def renameAll(b: Block, lhs: Map[Command, Map[Variable, Int]], rhs: Map[Command, Map[Variable, Int]]) = {
      for (s <- b.statements) {
        visit_stmt(StmtRenamer(lhs.get(s).getOrElse(Map()), rhs.get(s).getOrElse(Map())), s)
      }
      val s = b.jump
      visit_jump(StmtRenamer(lhs.get(s).getOrElse(Map()), rhs.get(s).getOrElse(Map())), s)
    }

    def visitProc(p: Procedure) = {
      /*
       * visit in weak topological order, collect renames and copies of ssa variables then apply transform
       * we need to visit loops twice to handle joins
       */

      val worklist = mutable.PriorityQueue[Block]()(Ordering.by(b => b.rpoOrder))
      worklist.addAll(p.blocks)
      var seen = Set[Block]()
      val count = mutable.Map[Variable, Int]().withDefaultValue(0)

      // ssa index to rename lvars and rvars respectively
      var lhs = Map[Command, Map[Variable, Int]]().withDefaultValue(Map().withDefaultValue(-1))
      var rhs = Map[Command, Map[Variable, Int]]().withDefaultValue(Map().withDefaultValue(-1))

      // the variable renaming at the end of each block
      var st = Map[Block, DSARes]().withDefaultValue(DSARes())

      while (worklist.nonEmpty) {
        val b = worklist.dequeue

        if (b.prevBlocks.forall(st.contains(_)) && !b.prevBlocks.forall(b => seen.contains(b))) {
          worklist.addAll(b.prevBlocks)
          worklist.enqueue(b)
        } else {
          val prev = if (b.prevBlocks.size > 1 && b.prevBlocks.forall(b => st.contains(b))) {
            // if we have a (possibly incomplete) entry for the incoming blocks we join them  
            njoin(st, b.prevBlocks)
          } else if (b.incomingJumps.size == 1) {
            st(b.incomingJumps.head.parent)
          } else {
            DSARes()
          }
          val (processed, nlhs, nrhs) = processCollect(prev, b, count, lhs, rhs)
          if (st(b) != processed || nlhs != lhs || nrhs != rhs) {
            lhs = nlhs
            rhs = nrhs
            worklist.addAll(b.nextBlocks)
            st = st.updated(b, processed)
          }
        }
        seen += b
      }
      val (lhss, rhss) = fixJoins(st, p, lhs, rhs)
      lhs = lhss
      rhs = rhss

      for (b <- p.blocks) {
        renameAll(b, lhs, rhs)
      }

    }

    applyRPO(program)
    program.procedures.foreach(visitProc)
  }
}



def undoDSA(p: Procedure) : Unit = {
  /**
   * This just naively removes the indices from variables, some analyses may require dsa form to maintain correctness
   * (e.g. bitvector width removal)
   *
   * You can only do this if the ssa index order matches the flow order, and all have the same type. 
   */
  visit_proc(RevIndices, p)
  visit_proc(RemoveCopy, p)

  object RevIndices extends CILVisitor {
    override def vlvar(v: Variable) = v match {
      case l: LocalVar => ChangeTo(LocalVar(l.varName, l.irType))
      case o => SkipChildren()
    }
    override def vrvar(v: Variable) = v match {
      case l: LocalVar => ChangeTo(LocalVar(l.varName, l.irType))
      case o => SkipChildren()
    }
  }

  object RemoveCopy extends CILVisitor {
    override def vstmt(s: Statement) = s match {
      case Assign(LocalVar(n1, t1), LocalVar(n2, t2), _) if n1 == n2 => ChangeTo(List())
      case o => SkipChildren()
    }
  }
}

def undoDSA(p: Program) : Unit = {
  for (proc <- p.procedures) {
    undoDSA(proc)
  }
}

