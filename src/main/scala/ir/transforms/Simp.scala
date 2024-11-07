package ir.transforms
import translating.serialiseIL

import util.Logger
import ir.eval.AlgebraicSimplifications
import ir.cilvisitor.*
import ir.*
import scala.collection.mutable
import analysis._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.*
import scala.util.{Failure, Success}
import ExecutionContext.Implicits.global

trait AbstractDomain[L] {
  def join(a: L, b: L, pos: Block): L
  def widen(a: L, b: L, pos: Block): L = join(a, b, pos)
  def narrow(a: L, b: L): L = a
  def transfer(a: L, b: Command): L
  def transferBlockFwd(a: L, b: Block): L = {
    transfer(b.statements.foldLeft(a)(transfer), b.jump)
  }
  def transferBlockBwd(a: L, b: Block): L = {
    b.statements.toList.reverse.foldLeft(transfer(a, b.jump))(transfer)
  }

  def top: L
  def bot: L
}

def getLiveVars(p: Procedure) : (Map[Block, Set[Variable]], Map[Block, Set[Variable]]) = {
  val liveVarsDom = IntraLiveVarsDomain()
  val liveVarsSolver = worklistSolver(liveVarsDom)
  liveVarsSolver.solveProc(p, backwards = true)
}

def difftestLiveVars(p: Procedure, compareResult: Map[CFGPosition, Set[Variable]]) = {
  val (liveBefore, liveAfter) = getLiveVars(p)
  var passed = true

  for ((b, s) <- liveBefore) {
    val c = (compareResult(b) == s)
    passed = passed && c
    if (!c) {
      Logger.error(
        s"LiveVars unequal ${b.label}: ${compareResult(b)} == $s (differing ${compareResult(b).diff(s)})"
      )
    }
  }
  passed
}

def basicReachingDefs(p: Procedure): Map[Command, Map[Variable, Set[Assign | DirectCall]]] = {
  val (beforeLive, afterLive) = getLiveVars(p)
  val dom = DefUseDomain(beforeLive)
  val solver = worklistSolver(dom)
  // type rtype = Map[Block, Map[Variable, Set[Assign | DirectCall]]]
  val (beforeRes, afterRes) = solver.solveProc(p)

  val merged: Map[Command, Map[Variable, Set[Assign | DirectCall]]] =
    beforeRes
      .flatMap((block, sts) => {
        val b = Seq(IRWalk.firstInBlock(block) -> sts)
        val stmts =
          if (block.statements.nonEmpty) then
            (block.statements.toList: List[Command]).zip(block.statements.toList.tail ++ List(block.jump))
          else List()
        val transferred = stmts
          .foldLeft((sts, List[(Command, Map[Variable, Set[Assign | DirectCall]])]()))((st, s) => {
            // map successor to transferred predecessor
            val x = dom.transfer(st._1, s._1)
            (x, (s._2 -> x) :: st._2)
          })
          ._2
          .toMap
        b ++ transferred
      })
      .toMap
  merged
}

case class DefUse(defined: Map[Variable, Assign | DirectCall])

// map v -> definitions reached here
class DefUseDomain(liveBefore: Map[Block, Set[Variable]])
    extends AbstractDomain[Map[Variable, Set[Assign | DirectCall]]] {
  // TODO: cull values using liveness

  override def transfer(s: Map[Variable, Set[Assign | DirectCall]], b: Command) = {
    b match {
      case a: Assign     => s.updated(a.lhs, Set(a))
      case d: DirectCall => d.outParams.map(_._2).foldLeft(s)((s, r) => s.updated(r, Set(d)))
      case _             => s
    }
  }
  override def top = ???
  def bot = Map[Variable, Set[Assign | DirectCall]]()
  def join(l: Map[Variable, Set[Assign | DirectCall]], r: Map[Variable, Set[Assign | DirectCall]], pos: Block) = {
    l.keySet
      .union(r.keySet)
      .filter(k => liveBefore(pos).contains(k))
      .map(k => {
        k -> (l.get(k).getOrElse(Set()) ++ r.get(k).getOrElse(Set()))
      })
      .toMap
  }

}

class CollectingDomain[T, D <: AbstractDomain[T]](d: D) extends AbstractDomain[(T, Map[Command, T])] {
  def bot = (d.bot, Map())
  def top = ???

  def join(l: (T, Map[Command, T]), r: (T, Map[Command, T]), pos: Block): (T, Map[Command, T]) = {
    val nr = d.join(l._1, r._1, pos)
    (
      nr,
      (l._2.keySet ++ r._2.keySet)
        .map((s: Command) => {
          ((l._2.get(s), r._2.get(s)) match {
            case (Some(l), Some(r)) if l == r => Seq(s -> l)
            case (Some(l), Some(r)) if l != r => Seq()
            case (Some(l), None)              => Seq(s -> l)
            case (None, Some(l))              => Seq(s -> l)
            case _                            => ???
          })
        })
        .flatten
        .toMap
    )
  }

  override def transfer(s: (T, Map[Command, T]), c: Command) = {
    val u = d.transfer(s._1, c)
    (u, s._2.updated(c, u))
  }
}

trait PowerSetDomain[T] extends AbstractDomain[Set[T]] {
  def bot = Set()
  def top = ???
  def join(a: Set[T], b: Set[T], pos: Block) = a.union(b)
}

class IntraLiveVarsDomain extends PowerSetDomain[Variable] {
  // expected backwards

  def transfer(s: Set[Variable], a: Command): Set[Variable] = {
    a match {
      case a: Assign       => (s - a.lhs) ++ a.rhs.variables
      case m: MemoryAssign => s ++ m.index.variables ++ m.value.variables
      case a: Assume       => s ++ a.body.variables
      case a: Assert       => s ++ a.body.variables
      case i: IndirectCall => s + i.target
      case c: DirectCall   => (s -- c.outParams.map(_._2)) ++ c.actualParams.flatMap(_._2.variables)
      case g: GoTo         => s
      case r: Return       => s ++ r.outParams.flatMap(_._2.variables)
      case r: Unreachable  => s
    }
  }
}

def removeSlices(p: Program): Unit = {
  p.procedures.foreach(removeSlices)
}

def circularDeps(p: Procedure): Set[Variable] = {

  /** this is flow insensitive, used to remove copy prop candidates which produce a substitution cycle
    */

  case class VTerm(v: Variable) extends analysis.solvers.Var[VTerm]

  // map variable to the representative of the set of dependencies
  val results = mutable.Map[Variable, VTerm]()
  val ufsolver = analysis.solvers.UnionFindSolver[VTerm]()

  def addDependency(lhs: Variable, dependency: Variable) = {
    val rep = results.get(lhs) match {
      case Some(d) => {
        ufsolver.unify(d, VTerm(dependency))
        val nrep = ufsolver.find(d).asInstanceOf[VTerm]
        results(lhs) = nrep
      }
      case None => VTerm(dependency)
    }
  }

  p.foreach {
    case (Assign(lhs: LocalVar, rhs, _)) => {
      for (rvar <- rhs.variables) {
        addDependency(lhs, rvar)
      }
    }
    case d: DirectCall => {
      // unify formal and actual
      for (rvar <- d.actualParams) {
        for (r <- rvar._2.variables) {
          addDependency(rvar._1, r)
        }
      }
      for (lvar <- d.outParams) {
        addDependency(lvar._1, lvar._2)
        for (rvar <- d.actualParams.flatMap(_._2.variables)) {
          // unify in and out
          addDependency(lvar._2, rvar)
        }
      }
    }
    case _ => ()
  }

  val unif = ufsolver.unifications().map((k, v) => (k, v.toSet)).toMap
  val circular = results
    .filter((v, rep) => {
      unif(rep).contains(VTerm(v))
    })
    .map((v, rep) => v)
    .toSet

  circular

}

def removeSlices(p: Procedure): Unit = {
  case class LVTerm(v: LocalVar) extends analysis.solvers.Var[LVTerm]

  /** if for each variable v there is some i:int such that (i) all its assignments have a ZeroExtend(i, x) and (ii) all
    * its uses have Extract(size(v) - i, 0, v) Then we replace v by a variable of bitvector size (size(v) - i)
    *
    * We check this flow-insensitively and recover precision using DSA form.
    */

  val assignments: Map[LocalVar, Iterable[Assign]] = p
    .collect { case a: Assign =>
      a
    }
    .groupBy(_.lhs)
    .collect { case (k: LocalVar, v) =>
      (k, v)
    }

  enum HighZeroBits:
    case Bits(
        n: Int,
        var unifromAccesses: Boolean = true,
        highAlwaysZero: Option[Int] = None
    ) // most significant bit that is accessed (and all below)
    case False // property is false
    case Bot // don't know anything

  // unify variable uses across direct assignments
  val ufsolver = analysis.solvers.UnionFindSolver[LVTerm]()
  assignments.foreach {
    case (lv, Assign(lhs: LocalVar, rhs: LocalVar, _)) => ufsolver.unify(LVTerm(lhs), LVTerm(rhs))
    case _                                             => ()
  }

  def getRep(v: LocalVar): LocalVar = ufsolver.find(LVTerm(v)).asInstanceOf[LVTerm].v

  val unifiedAssignments = ufsolver
    .unifications()
    .map {
      case (v: LVTerm, rvs) =>
        v.v -> (rvs.map { case LVTerm(rv) =>
          rv
        }).toSet
      case _ => ???
    }
    .map((repr: LocalVar, elems: Set[LocalVar]) =>
      repr -> elems.flatMap(assignments(_).filter(_ match {
        // filter out the direct assignments we used to build the unif class
        case Assign(lhs: LocalVar, rhs: LocalVar, _) if elems.contains(lhs) && elems.contains(rhs) => false
        case _                                                                                     => true
      }))
    )

  class CheckUsesHaveExtend() extends CILVisitor {
    val result: mutable.HashMap[LocalVar, HighZeroBits] =
      mutable.HashMap[LocalVar, HighZeroBits]()

    def extractAccess(actualVar: LocalVar, highestBit: Int): Unit = {
      val v = getRep(actualVar)
      // val v = actualVar
      if (!size(v).isDefined) {
        return ();
      }

      if (highestBit == size(v).get) {
        result(v) = HighZeroBits.False
      } else if (((!result.contains(v)) || result.get(v).contains(HighZeroBits.Bot))) {
        result(v) = HighZeroBits.Bits(highestBit, true)
      } else {
        result(v) match {
          case HighZeroBits.Bits(n, _, z) if highestBit > n && n != 0 => {
            // relax constraint to bits accessed
            result(v) = HighZeroBits.Bits(highestBit, false, z)
          }
          case HighZeroBits.Bits(n, _, _) if n >= highestBit => {
            // access satisfied by upper constraint
          }
          case _ => ()
        }
      }
    }

    // TODO: make correct wrt casting if you know that you can extend to zero
    //  - otherwise dont smallen variable
    //  - allow casting at more points (e.g. copies)

    override def vstmt(s: Statement) = {
      s match {
        case Assign(l: LocalVar, r: LocalVar, _) => {

          /** direct copy ; use union-find result to process access * */
          ufsolver.unify(LVTerm(l), LVTerm(r))
          SkipChildren()
        }
        case Assign(v: LocalVar, ZeroExtend(sz, exp), _) => {
          val l = getRep(v)
          result.get(l) match {
            case None => {
              result(l) = HighZeroBits.Bits(0, true, Some(sz))
            }
            case Some(HighZeroBits.Bits(bits, o, None)) => {
              result(l) = HighZeroBits.Bits(bits, o, Some(sz))
            }
            case Some(HighZeroBits.Bits(bits, o, Some(existing))) if existing <= sz => {
              result(l) = HighZeroBits.Bits(bits, o, Some(existing))
            }
            case Some(HighZeroBits.Bits(bits, o, Some(existing))) if existing > sz => {
              result(l) = HighZeroBits.Bits(bits, o, Some(sz))
            }
          }
          DoChildren()
        }
        case Assign(v: LocalVar, exp, _) => {
          val l = getRep(v)
          result.get(l) match {
            case None => {
              result(l) = HighZeroBits.Bits(0, true, Some(0))
            }
            case Some(HighZeroBits.Bot) => {
              result(l) = HighZeroBits.Bits(0, true, Some(0))
            }
            case Some(HighZeroBits.Bits(bits, o, Some(existing))) => {
              result(l) = HighZeroBits.Bits(bits, o, Some(0))
            }
            case _ => {}
          }
          DoChildren()
        }
        case d: DirectCall => {

          /** we always need to pass whole variable across calls since we analyse intraprocedurally */
          d.outParams.map(_._2).collect {
            case l: LocalVar => {
              extractAccess(l, size(l).get)
            }
          }
          d.actualParams.flatMap(_._2.variables).collect {
            case l: LocalVar => {
              extractAccess(l, size(l).get)
            }
          }
          SkipChildren()
        }
        case _ => DoChildren()
      }
    }

    override def vjump(j: Jump) = j match {
      case r: Return => {
        for ((formal, actual) <- r.outParams) {
          actual match {
            case v: LocalVar if (size(v).isDefined) => {
              extractAccess(v, size(formal).get)
            }
            case _ => ()
          }
        }
        SkipChildren()
      }
      case _ => DoChildren()
    }

    override def vrvar(v: Variable) = {
      v match {
        case v: LocalVar if size(v).isDefined => {
          result(getRep(v)) = HighZeroBits.False
        }
        case _ => ()
      }
      SkipChildren()
    }

    override def vexpr(v: Expr) = {
      v match {
        case Extract(i, 0, v: LocalVar) if size(v).isDefined => {
          result(getRep(v)) = HighZeroBits.False
          SkipChildren()
        }
        case _ => DoChildren()
      }
    }

    def apply(p: Procedure): Map[LocalVar, HighZeroBits] = {
      result.clear()
      visit_proc(this, p)
      result.toMap
    }
  }

  val onlyKeepWhereAllAccessesSliceSameBits = true
  val toSmallen = CheckUsesHaveExtend()(p).collect { case (v, HighZeroBits.Bits(x, _, _)) =>
    v -> x
  }.toMap

  /** This transform moves bvextracts from the uses to the definition, if all uses have an extract of some size. Ideally
    * this removes the extract in some cases. To bemore strict the val onlyKeepWhereAllAccessesSliceSameBits = true flag
    * only applies this transform when all accesses have extactly the same slice, effectively removing the slice at
    * access.
    */

  class ReplaceAlwaysSlicedVars(varHighZeroBits: Map[LocalVar, Int]) extends CILVisitor {
    var formals = Set[LocalVar]()

    override def vexpr(v: Expr) = {
      v match {
        case Extract(i, 0, v: LocalVar)
            if size(v).isDefined && !(formals.contains(v)) && varHighZeroBits.contains(getRep(v)) => {
          val size = varHighZeroBits(getRep(v))
          assert(size >= i)
          if (size == i) {
            ChangeTo(v.copy(irType = BitVecType(size)))
          } else {
            ChangeTo(Extract(i, 0, v.copy(irType = BitVecType(size))))
          }
        }
        case _ => DoChildren()
      }
    }

    override def vlvar(v: Variable) = {
      v match {
        case lhs: LocalVar if (varHighZeroBits.contains(getRep(lhs)) && !formals.contains(getRep(lhs))) => {
          val n = lhs.copy(irType = BitVecType(varHighZeroBits(getRep(lhs))))
          ChangeTo(n)
        }
        case _ => SkipChildren()
      }
    }

    override def vrvar(v: Variable) = {
      v match {
        case lhs: LocalVar if (varHighZeroBits.contains(getRep(lhs)) && !formals.contains(getRep(lhs))) => {
          val n = lhs.copy(irType = BitVecType(varHighZeroBits(getRep(lhs))))
          ChangeTo(n)
        }
        case _ => SkipChildren()
      }
    }

    override def vproc(p: Procedure) = {
      formals = p.formalInParam.toSet ++ p.formalOutParam.toSet
      DoChildren()
    }

    override def vstmt(s: Statement) = {
      s match {
        case a @ Assign(lhs: LocalVar, rhs: LocalVar, _)
            if varHighZeroBits.contains(getRep(lhs)) && varHighZeroBits.contains(getRep(rhs)) => {
          assert(varHighZeroBits(getRep(lhs)) == varHighZeroBits(getRep(rhs)))
          a.lhs = lhs.copy(irType = BitVecType(varHighZeroBits(getRep(lhs))))
          a.rhs = rhs.copy(irType = BitVecType(varHighZeroBits(getRep(rhs))))
          // if (varHighZeroBits(getRep(lhs)) == varHighZeroBits(getRep(rhs))) {
          // } else if (varHighZeroBits(lhs) < varHighZeroBits(rhs)) {
          //   a.lhs = lhs.copy(irType=BitVecType(varHighZeroBits(lhs)))
          //   a.rhs = Extract(varHighZeroBits(lhs), 0, rhs.copy(irType=BitVecType(varHighZeroBits(rhs))))
          // } else {
          //   // lhs > rhs
          //   a.lhs = lhs.copy(irType=BitVecType(varHighZeroBits(lhs)))
          //   a.rhs = ZeroExtend(varHighZeroBits(lhs) - varHighZeroBits(rhs),rhs.copy(irType=BitVecType(varHighZeroBits(rhs))))
          // }
          SkipChildren()
        }
        case a @ Assign(lhs: LocalVar, SignExtend(sz, rhs), _)
            if size(lhs).isDefined && varHighZeroBits.get(getRep(lhs)).contains(size(rhs).get) && !(formals
              .contains(lhs)) => {
          // assert(varHighZeroBits(lhs) == sz)
          val varsize = varHighZeroBits(getRep(lhs))
          a.lhs = LocalVar(lhs.varName, BitVecType(varsize), lhs.index)
          a.rhs = rhs
          assert(size(a.lhs).get == size(a.rhs).get)
          DoChildren()
        }
        case a @ Assign(lhs: LocalVar, ZeroExtend(sz, rhs), _)
            if size(lhs).isDefined && varHighZeroBits.get(getRep(lhs)).contains(size(rhs).get) && !(formals
              .contains(lhs)) => {
          val varsize = varHighZeroBits(getRep(lhs))
          a.lhs = LocalVar(lhs.varName, BitVecType(varsize), lhs.index)
          a.rhs = rhs
          assert(size(a.lhs).get == size(a.rhs).get)
          DoChildren()
        }
        case a @ Assign(lhs: LocalVar, rhs, _)
            if size(lhs).isDefined && varHighZeroBits.contains(getRep(lhs)) && !(formals.contains(lhs)) => {
          // promote extract to the definition
          val varsize = varHighZeroBits(getRep(lhs))
          a.lhs = LocalVar(lhs.varName, BitVecType(varsize), lhs.index)
          a.rhs = Extract(varsize, 0, rhs)
          assert(size(a.lhs).get == size(a.rhs).get, s"${size(a.lhs).get} != ${size(a.rhs).get}")
          DoChildren()
        }
        case _ => DoChildren()
      }
    }
  }

  visit_proc(ReplaceAlwaysSlicedVars(toSmallen), p)

}

def getRedundantAssignments(procedure: Procedure): Set[Assign] = {

  /** Get all assign statements which define a variable never used, assuming ssa form and proc parameters so that
    * interprocedural check is not required.
    */

  enum VS:
    case Bot
    case Assigned(definition: Set[Assign])
    case Read(definition: Set[Assign], uses: Set[CFGPosition])

  def joinVS(a: VS, b: VS) = {
    (a, b) match {
      case (VS.Bot, o)                        => o
      case (o, VS.Bot)                        => o
      case (VS.Read(d, u), VS.Read(d1, u1))   => VS.Read(d ++ d1, u ++ u1)
      case (VS.Assigned(d), VS.Read(d1, u1))  => VS.Read(d ++ d1, u1)
      case (VS.Read(d1, u1), VS.Assigned(d))  => VS.Read(d ++ d1, u1)
      case (VS.Assigned(d1), VS.Assigned(d2)) => VS.Assigned(d1 ++ d2)
    }
  }

  val assignedNotRead = mutable.Map[Variable, VS]().withDefaultValue(VS.Bot)

  for (c <- procedure) {
    c match {
      case a: Assign => {
        assignedNotRead(a.lhs) = joinVS(assignedNotRead(a.lhs), VS.Assigned(Set(a)))
        a.rhs.variables.foreach(v => {
          assignedNotRead(v) = joinVS(assignedNotRead(v), VS.Read(Set(), Set(a)))
        })
      }
      case m: MemoryAssign => {
        m.index.variables.foreach(v => {
          assignedNotRead(v) = joinVS(assignedNotRead(v), VS.Read(Set(), Set(m)))
        })
        m.value.variables.foreach(v => {
          assignedNotRead(v) = joinVS(assignedNotRead(v), VS.Read(Set(), Set(m)))
        })
      }
      case m: IndirectCall => {
        assignedNotRead(m.target) = joinVS(assignedNotRead(m.target), VS.Read(Set(), Set(m)))
      }
      case m: Assert => {
        m.body.variables.foreach(v => {
          assignedNotRead(v) = joinVS(assignedNotRead(v), VS.Read(Set(), Set(m)))
        })
      }
      case m: Assume => {
        for (v <- m.body.variables) {
          assignedNotRead(v) = joinVS(assignedNotRead(v), VS.Read(Set(), Set(m)))
        }
      }
      case c: DirectCall => {
        c.actualParams
          .flatMap(_._2.variables)
          .foreach(v => {
            assignedNotRead(v) = joinVS(assignedNotRead(v), VS.Read(Set(), Set(c)))
          })
      }
      case p: Return => {
        p.outParams
          .flatMap(_._2.variables)
          .foreach(v => {
            assignedNotRead(v) = joinVS(assignedNotRead(v), VS.Read(Set(), Set(p)))
          })
      }
      case p: GoTo        => ()
      case p: NOP         => ()
      case p: Unreachable => ()
      case p: Procedure   => ()
      case b: Block       => ()
    }
  }

  var toRemove = assignedNotRead
  var removeOld = toRemove

  // def remove(a: Assign): Boolean = {
  //   var removed : Boolean = false
  //   toRemove = toRemove.map((v, s) =>
  //     v -> {
  //       s match {
  //         case VS.Read(defs, uses) if uses.size == 1 && uses.contains(a) => {
  //           removed = true
  //           VS.Assigned(defs)
  //         }
  //         case VS.Read(defs, uses) if uses.contains(a)                   => {
  //           removed = true
  //           VS.Read(defs, uses - a)
  //         }
  //         case o                                                         => o
  //       }
  //     }
  //   )
  //   removed
  // }

  // while ({
  //   removeOld = toRemove
  //   val removed = removeOld.map((v, s) => {
  //     s match {
  //       case VS.Assigned(definition) => definition.map(remove).foldLeft(false)((x,y) => x || y)
  //       case _                       => false
  //     }
  //   })
  //   removed.exists(x => x)
  // }) {}

  val r = toRemove
    .collect { case (v, VS.Assigned(d)) =>
      d
    }
    .toSet
    .flatten
  r
}

class CleanupAssignments() extends CILVisitor {
  var redundantAssignments = Set[Assign]()

  def isRedundant(a: Assign) = {
    a.lhs == a.rhs || redundantAssignments.contains(a)
  }

  override def vproc(p: Procedure) = {
    redundantAssignments = getRedundantAssignments(p)
    DoChildren()
  }

  override def vstmt(s: Statement) = s match {
    case a: Assign if isRedundant(a) => ChangeTo(List())
    case _                           => SkipChildren()
  }

}

def copypropTransform(p: Procedure) = {
  val t = util.PerformanceTimer(s"simplify ${p.name} (${p.blocks.size} blocks)")
  // val dom = ConstCopyProp()
  // val solver = worklistSolver(dom)

  // Logger.info(s"${p.name} ExprComplexity ${ExprComplexity()(p)}")
  // val result = solver.solveProc(p, true).withDefaultValue(dom.bot)
  val result = CopyProp.DSACopyProp(p)
  val solve = t.checkPoint("Solve CopyProp")

  if (result.nonEmpty) {
    val vis = Simplify(CopyProp.toResult(result, true))
    visit_proc(vis, p)

    val condResult = CopyProp.PropFlagCalculations(p, result.toMap)
    val condVis = Simplify(CopyProp.toResult(condResult, false))
    visit_proc(condVis, p)

  }

  val xf = t.checkPoint("transform")
  // Logger.info(s"    ${p.name} after transform expr complexity ${ExprComplexity()(p)}")

  visit_proc(CleanupAssignments(), p)
  t.checkPoint("redundant assignments")
  // Logger.info(s"    ${p.name} after dead var cleanup expr complexity ${ExprComplexity()(p)}")

  visit_proc(AlgebraicSimplifications, p)
  visit_proc(AlgebraicSimplifications, p)
  visit_proc(AlgebraicSimplifications, p)
  visit_proc(AlgebraicSimplifications, p)
  ir.eval.cleanupSimplify(p)
  // visit_proc(AlgebraicSimplifications, p)
  // visit_proc(AlgebraicSimplifications, p)
  // visit_proc(AlgebraicSimplifications, p)
  // visit_proc(AlgebraicSimplifications, p)
  // visit_proc(AlgebraicSimplifications, p)
  // visit_proc(AlgebraicSimplifications, p)
  // visit_proc(AlgebraicSimplifications, p)
  // visit_proc(AlgebraicSimplifications, p)
  // Logger.info(s"    ${p.name}  after simp expr complexity ${ExprComplexity()(p)}")
  val sipm = t.checkPoint("algebraic simp")

  // Logger.info("[!] Simplify :: RemoveSlices")
  // removeSlices(p)
  visit_proc(AlgebraicSimplifications, p)

}

def removeEmptyBlocks(p: Program) = {
  for (proc <- p.procedures) {
    val blocks = proc.blocks.toList
    for (b <- blocks) {
      b match {
        case b: Block if b.statements.size == 0 && b.prevBlocks.size == 1 && b.jump.isInstanceOf[GoTo] => {
          val prev = b.prevBlocks
          val next = b.nextBlocks
          for (p <- prev) {
            p.jump match {
              case g: GoTo => {
                for (n <- next) {
                  g.addTarget(n)
                }
                g.removeTarget(b)
              }
              case _ => throw Exception("Must have goto")
            }
          }
          b.replaceJump(Unreachable())
          b.parent.removeBlocks(b)
        }
        case _ => ()
      }
    }
  }
}

def coalesceBlocks(p: Program) = {
  var didAny = false
  for (proc <- p.procedures) {
    val blocks = proc.blocks.toList
    for (b <- blocks.sortBy(_.rpoOrder)) {
      if (
        b.prevBlocks.size == 1 && b.prevBlocks.head.statements.nonEmpty && b.statements.nonEmpty
        && b.prevBlocks.head.nextBlocks.size == 1
        && b.prevBlocks.head.statements.lastOption.map(s => !(s.isInstanceOf[Call])).getOrElse(true)
        && !(b.parent.entryBlock.contains(b) || b.parent.returnBlock.contains(b))
      ) {
        didAny = true
        // append topredecessor
        // we know prevBlock is only jumping to b and has no call at the end
        val prevBlock = b.prevBlocks.head
        val stmts = b.statements.map(b.statements.remove).toList
        prevBlock.statements.appendAll(stmts)
        // leave empty block b and cleanup with removeEmptyBlocks
      } else if (
        b.nextBlocks.size == 1 && b.nextBlocks.head.statements.nonEmpty && b.statements.nonEmpty
        && b.nextBlocks.head.prevBlocks.size == 1
        && b.statements.lastOption.map(s => !(s.isInstanceOf[Call])).getOrElse(true)
        && !(b.parent.entryBlock.contains(b) || b.parent.returnBlock.contains(b))
      ) {
        didAny = true
        // append to successor
        // we know b is only jumping to nextBlock and does not end in a call
        val nextBlock = b.nextBlocks.head
        val stmts = b.statements.map(b.statements.remove).toList
        nextBlock.statements.prependAll(stmts)
        // leave empty block b and cleanup with removeEmptyBlocks
      }
    }
  }
  didAny
}

def doCopyPropTransform(p: Program) = {

  applyRPO(p)


  Logger.info("[!] Simplify :: Expr/Copy-prop Transform")
  val work = p.procedures
    .filter(_.blocks.size > 0)
    .map(p =>
      p -> //Future
        {
          Logger
            .debug(s"CopyProp Transform ${p.name} (${p.blocks.size} blocks, expr complexity ${ExprComplexity()(p)})")
          copypropTransform(p)
        }
    )

  work.foreach((p, job) => {
    try {
      //Await.result(job, 10000.millis)
      job
    } catch {
      case e => {
        Logger.error("Simplify :: CopyProp " + p.name + ": " + e.toString)
      }
    }
  })

  Logger.info("[!] Simplify :: Dead variable elimination")

  // cleanup
  visit_prog(CleanupAssignments(), p)
  Logger.info("[!] Simplify :: Merge empty blocks")

  removeEmptyBlocks(p)
  coalesceBlocks(p)
  removeEmptyBlocks(p)
  coalesceBlocks(p)
  removeEmptyBlocks(p)
  coalesceBlocks(p)
  removeEmptyBlocks(p)
  coalesceBlocks(p)
  removeEmptyBlocks(p)

}

def reversePostOrder(startBlock: Block): Unit = {
  var count = 0
  val seen = mutable.HashSet[Block]()
  val vcs = mutable.HashMap[Block, Int]()

  def walk(b: Block): Unit = {
    seen += b
    for (s <- b.nextBlocks) {
      if (!seen.contains(s)) {
        walk(s)
      }
    }
    b.rpoOrder = count
    count += 1
  }

  walk(startBlock)
}

def applyRPO(p: Program) = {
  for (proc <- p.procedures) {
    proc.entryBlock.map(eb => reversePostOrder(eb))
  }
}

class worklistSolver[L, A <: AbstractDomain[L]](domain: A) {

  def solveProc(p: Procedure, backwards: Boolean = false) = {
    solve(p.blocks, Set(), Set(), backwards)
  }

  def solveProg(
      p: Program,
      widenpoints: Set[Block], // set of loop heads
      narrowpoints: Set[Block] // set of conditions
  ): Map[Procedure, Map[Block, L]] = {
    val initDom = p.procedures.map(p => (p, p.blocks))

    val work = initDom.map(d => {
      (
        d._1,
        Future {
          val t = util.PerformanceTimer(s"solve ${d._1.name}")
          Logger.info(s"begin ${t.timerName}")
          val r = solve(d._2, Set(), Set())
          t.checkPoint("finished")
          r
        }
      )
    })
    work
      .map((prog, x) =>
        try {
          (prog, Await.result(x, 10000.millis)._2)
        } catch {
          case t: Exception => {
            Logger.error(s"${prog.name} : $t")
            (prog, Map())
          }
        }
      )
      .toMap
    // Await.result(Future.sequence(work), Duration.Inf).toMap
  }

  def solve(
      initial: IterableOnce[Block],
      widenpoints: Set[Block], // set of loop heads
      narrowpoints: Set[Block], // set of conditions
      backwards: Boolean = false
  ): (Map[Block, L], Map[Block, L]) = {
    val savedAfter: mutable.HashMap[Block, L] = mutable.HashMap()
    val savedBefore: mutable.HashMap[Block, L] = mutable.HashMap()
    val saveCount: mutable.HashMap[Block, Int] = mutable.HashMap()
    val worklist = {
      if (backwards) {
        mutable.PriorityQueue[Block]()(Ordering.by(b => -b.rpoOrder))
      } else {
        mutable.PriorityQueue[Block]()(Ordering.by(b => b.rpoOrder))
      }
    }
    worklist.addAll(initial)

    def successors(b: Block) = if backwards then b.prevBlocks else b.nextBlocks
    def predecessors(b: Block) = if backwards then b.nextBlocks else b.prevBlocks

    while (worklist.nonEmpty) {
      val b = worklist.dequeue

      while (
        worklist.nonEmpty && (if backwards then (worklist.head.rpoOrder <= b.rpoOrder)
                              else (worklist.head.rpoOrder >= b.rpoOrder))
      ) do {
        // drop rest of blocks with same priority
        val m = worklist.dequeue()
        assert(
          m == b,
          s"Different nodes with same priority ${m.rpoOrder} ${b.rpoOrder}, violates PriorityQueueWorklist assumption: $b and $m"
        )
      }

      val prev = savedAfter.get(b)
      val x = {
        predecessors(b).toList.flatMap(b => savedAfter.get(b).toList) match {
          case Nil      => domain.bot
          case h :: Nil => h
          case h :: tl  => tl.foldLeft(h)((acc, nb) => domain.join(acc, nb, b))
        }
      }
      savedBefore(b) = x
      val todo = List(b)

      val lastBlock = b // todo.last
      var nx = todo.foldLeft(x)((x, b) => {
        savedBefore(b) = x
        if (backwards) {
          val ojmp = domain.transfer(x, b.jump)
          savedAfter(b) = b.statements.toList.reverse.foldLeft(ojmp)(domain.transfer)
        } else {
          val stmts = b.statements.foldLeft(x)(domain.transfer)
          savedAfter(b) = domain.transfer(stmts, b.jump)
        }
        savedAfter(b)
      })
      savedAfter(lastBlock) = nx
      saveCount(lastBlock) = saveCount.get(lastBlock).getOrElse(0) + 1
      if (!prev.contains(nx)) then {
        if (saveCount(lastBlock) >= 50) {
          Logger.warn(s"Large join count on block ${lastBlock.label}, no fix point? (-v for mor info)")
          Logger.debug(lastBlock.label + "    ==> " + x)
          Logger.debug(lastBlock.label + "    <== " + nx)
        }
        worklist.addAll(successors(lastBlock))
      }
    }
    if backwards then (savedAfter.toMap, savedBefore.toMap) else (savedBefore.toMap, savedAfter.toMap)
  }
}

// case class CopyProp(from: Expr, expr: Expr, deps: Set[Variable])

enum CopyProp {
  case Bot
  case Prop(expr: Expr, deps: Set[Variable])
  case Clobbered
}

case class CCP(
    val state: Map[Variable, CopyProp] = Map()
)

object CCP {

  def toSubstitutions(c: CCP): Map[Variable, Expr] = {
    c.state.collect { case (v, CopyProp.Prop(e, _)) =>
      v -> e
    }
  }

  def clobberFull(c: CCP, l: Variable) = {
    val p = clobber(c, l)
    p.copy(state = p.state + (l -> CopyProp.Clobbered))
  }

  def clobber(c: CCP, l: Variable) = {
    CCP(
      c.state
        .map((k, v) =>
          k -> (v match {
            case CopyProp.Prop(_, deps) if deps.contains(l) => CopyProp.Clobbered
            case o                                          => o
          })
        )
        .withDefaultValue(CopyProp.Bot)
    )
  }
}

object CopyProp {


  def forwardFlagCalc(p: Procedure) = {
    val (beforeLive, afterLive) = getLiveVars(p)
    val dom = DefUseDomain(beforeLive)
    val solver = worklistSolver(dom)
    // type rtype = Map[Block, Map[Variable, Set[Assign | DirectCall]]]
    val (beforeRes, afterRes) = solver.solveProc(p)

  }

  def isFlagVar(l: Variable) = {
    val flagNames = Set("ZF", "VF", "CF", "NF")
    l match {
      case l: LocalVar => flagNames.contains(l.varName)
      case l           => flagNames.contains(l.name)
    }
  }

  case class PropState(
      var e: Expr,
      val deps: mutable.Set[Variable],
      var clobbered: Boolean,
      var useCount: Int,
      var isFlagDep: Boolean,
  )


  def PropFlagCalculations(p: Procedure, initialState: Map[Variable, PropState]) = {
    val state = mutable.HashMap[Variable, PropState]()

    val flagDeps = initialState.collect {
      case (v, e) if e.isFlagDep => v -> e
    }

    val worklist = mutable.PriorityQueue[Block]()(Ordering.by(_.rpoOrder))
    worklist.addAll(p.blocks)
    var poisoned = false

    def transfer(s: Statement) = {
      s match {
        case a: Assign if a.rhs.loads.size > 0 => clobberFull(state, a.lhs)
        case a: Assign if !state.contains(a.lhs) && flagDeps.contains(a.lhs) => {
          val r = canPropTo(state, a.rhs, true).get
          state(a.lhs) = PropState(r, mutable.Set.from(r.variables), false, 0, true)
        }
        case a: Assign if state.contains(a.lhs) && state(a.lhs).clobbered => {
          ()
        }
        case a: Assign
            if state.contains(a.lhs) && (a.rhs != state(a.lhs).e) && (canPropTo(state, a.rhs, true) != canPropTo(state, state(a.lhs).e, true))
             => {
          clobberFull(state, a.lhs)
        }
        case a: Assign if state.contains(a.lhs) => {
          state(a.lhs) = state(a.lhs).copy(e = canPropTo(state, a.rhs, true).get)
        }
        case i: IndirectCall => {
          poisoned = true
        }
        case d: DirectCall => {
          for (l <- d.outParams.map(_._2)) {
            clobberFull(state, l)
          }
        }
        case _ => ()
      }
    }

    while (worklist.nonEmpty && !poisoned) {
      val b: Block = worklist.dequeue

      for (l <- b.statements) {
        transfer(l)
      }
    }
    if (poisoned) then mutable.HashMap() else state
  }

  def clobberFull(c: mutable.HashMap[Variable, PropState], l: Variable): Unit = {
    if (c.contains(l)) {
      c(l).clobbered = true
    } else {
      c(l) = PropState(FalseLiteral, mutable.Set(), true, 0, false)
    }
  }

  def clobberDeps(c: mutable.HashMap[Variable, PropState], l: Variable): Unit = {
    val toclobber = c.filter(_.isInstanceOf[CopyProp.Prop]).filter(_.asInstanceOf[CopyProp.Prop].deps.contains(l))
    for ((v, e) <- toclobber) {
      c(v).clobbered = true
    }
  }

  def isTrivial(e: Expr) = e match {
    case l: Literal                              => true
    case l: Variable                             => true
    case BinaryExpr(op, e: Variable, b: Literal) => true
    case _                                       => false
  }

  def canPropTo(s: mutable.HashMap[Variable, PropState], e: Expr, isFlag: Boolean = false): Option[Expr] = {

    def proped(e: Expr) = {
      val ne =
        if e.variables.size > 1 && !isFlag then Some(e)
        else
          Substitute(
            v => {
              s.get(v) match {
                case Some(vs) if !vs.clobbered => Some(vs.e)
                case _                         => None
              }
            },
            true
          )(e)

      // partial eval after prop
      eval.simplifyExprFixpoint(false)(ne.getOrElse(e))
    }

    def propfp(e: Expr) = {
      var o = e
      var p = proped(e)

      while (o != p) {
        o = p
        p = proped(o)
      }
      p
    }

    proped(e) match {
      case l: Literal                                 => Some(l)
      case l: Variable                                => Some(l)
      case e @ BinaryExpr(o, v: Variable, c: Literal) => Some(e)
      case e: Expr if isFlag || e.variables.size <= 1 => Some(e)
      case e                                          => None
    }
  }

  def DSACopyProp(p: Procedure) = {

    val state = mutable.HashMap[Variable, PropState]()
    var poisoned = false // we have an indirect call

    def transfer(c: mutable.HashMap[Variable, PropState], s: Statement): Unit = {
      // val callClobbers = ((0 to 7) ++ (19 to 30)).map("R" + _).map(c => Register(c, 64))
      s match {
        case Assign(l, r, lb) => {
          if (r.loads.size > 0) {
            // c.copy(state = c.state + (l -> CopyProp.Clobbered))
            clobberFull(c, l)
          } else {

            val isFlag = isFlagVar(l) || r.variables.exists(isFlagVar)
            val isFlagDep = isFlag || c.get(l).map(v => v.isFlagDep).getOrElse(false)

            c.get(l).foreach(v => v.isFlagDep = v.isFlagDep || isFlagDep)
            for (l <- r.variables) {
              c.get(l).foreach(v => v.isFlagDep = v.isFlagDep || isFlagDep)
            }

            var prop = canPropTo(c, r, isFlagDep)
            val existing = c.get(l)

            (prop, existing) match {
              case (Some(evaled), None) => {
                c(l) = PropState(evaled, mutable.Set.from(evaled.variables), false, 0, isFlagDep)
              }
              case (_, Some(ps)) if ps.clobbered => {
                ()
              }
              case (Some(evaled), Some(ps))
                  if ps.e == r || ps.e == evaled || (canPropTo(c, ps.e, isFlagDep).contains(evaled)) => {
                c(l) = c(l).copy(e = evaled)
              }
              case (Some(evaled), Some(ps)) => {
                clobberFull(c, l)
              }
              case _ => {
                // ps.e != evaled and have prop
                clobberFull(c, l)
              }
            }

          }
        }
        case x: DirectCall => {
          val lhs = x.outParams.map(_._2)
          for (l <- lhs) {
            clobberFull(c, l)
          }
        }
        case x: IndirectCall => {
          // not really correct
          poisoned = true
          for ((i, v) <- c) {
            v.clobbered = true
          }
        }
        case _ => ()
      }
    }

    // sort by precedence
    val worklist = mutable.PriorityQueue[Block]()(Ordering.by(_.rpoOrder))
    worklist.addAll(p.blocks)

    while (worklist.nonEmpty && !poisoned) {
      val b: Block = worklist.dequeue

      for (l <- b.statements) {
        transfer(state, l)
      }
    }

    val trivialOnly = false

    if poisoned then mutable.HashMap() else state
  }

  def toResult(s: mutable.Map[Variable, PropState], trivialOnly: Boolean = true)(v: Variable): Option[Expr] = {
    s.get(v) match {
      case Some(c) if !c.clobbered && (!trivialOnly || isTrivial(c.e)) => Some(c.e)
      case _                                                           => None
    }
  }

}

class ConstCopyProp() extends AbstractDomain[CCP] {
  private final val callClobbers = ((0 to 7) ++ (19 to 30)).map("R" + _).map(c => Register(c, 64))

  def top: CCP = CCP(Map().withDefaultValue(CopyProp.Clobbered))
  def bot: CCP = CCP(Map().withDefaultValue(CopyProp.Bot))

  override def join(l: CCP, r: CCP, pos: Block): CCP = {
    // val ks = l.state.keySet.intersect(r.state.keySet)
    val ks = l.state.keySet ++ (r.state.keySet)

    val merged = ks.map(v =>
      (v ->
        ((l.state.get(v).getOrElse(CopyProp.Clobbered), r.state.get(v).getOrElse(CopyProp.Clobbered)) match {
          case (l, CopyProp.Bot)                                                            => l
          case (CopyProp.Bot, r)                                                            => r
          case (c @ CopyProp.Clobbered, _)                                                  => c
          case (_, c @ CopyProp.Clobbered)                                                  => c
          case (p1 @ CopyProp.Prop(e1, deps1), p2 @ CopyProp.Prop(e2, deps2)) if (p1 == p2) => p1
          case (_, _)                                                                       => CopyProp.Clobbered
        }))
    )
    CCP(merged.toMap)
  }

  override def transfer(c: CCP, s: Command): CCP = {
    s match {
      case Assign(l, r, lb) => {
        if (r.loads.size > 0) {
          CCP.clobberFull(c, l)
        } else {
          val evaled = ir.eval.partialEvalExpr(
            Substitute(
              v => {
                c.state.get(v) match {
                  case Some(CopyProp.Prop(c, deps)) if deps.isEmpty => Some(c)
                  case _                                            => None
                }
              },
              false
            )(r).getOrElse(r),
            e => None
          )
          val rhsDeps = evaled.variables.toSet
          val existing = c.state.get(l).getOrElse(CopyProp.Bot)

          existing match {
            case CopyProp.Bot => {
              c.copy(state = c.state + (l -> CopyProp.Prop(evaled, rhsDeps))) // not seen yet
            }
            case CopyProp.Prop(e, _) => {
              val p = CCP.clobber(c, l)
              p.copy(state = p.state + (l -> CopyProp.Prop(evaled, rhsDeps))) // not seen yet
            }
            case _ => {
              CCP.clobberFull(c, l)
            }
          }
        }
      }
      case x: DirectCall => {
        val lhs = x.outParams.map(_._2)
        lhs.foldLeft(c)(CCP.clobberFull)
      }
      case x: IndirectCall => {
        val toClob = callClobbers
        toClob.foldLeft(c)(CCP.clobberFull)
      }
      case _ => c
    }
  }
}

class ExprComplexity extends CILVisitor {
  // count the nodes in the expression AST
  var count = 0
  override def vexpr(e: Expr) = {
    count += 1
    DoChildren()
  }

  def apply(e: Procedure) = {
    count = 0
    visit_proc(this, e)
    count
  }

  def apply(e: Expr) = {
    count = 0
    visit_expr(this, e)
    count
  }
}

class Substitute(
    val res: Variable => Option[Expr],
    val recurse: Boolean = true,
    val complexityThreshold: Int = 0
) extends CILVisitor {
  var madeAnyChange = false
  var complexity = 0

  override def vexpr(e: Expr) = {
    e match {
      case v: Variable if res(v).isDefined => {
        val changeTo = res(v).get
        if (complexityThreshold > 0) {
          complexity += ExprComplexity()(changeTo) - ExprComplexity()(e)
        }
        if (complexityThreshold > 0 && complexity > complexityThreshold) {
          SkipChildren()
        } else if (recurse) {
          madeAnyChange = true
          ChangeDoChildrenPost(changeTo, x => x)
        } else {
          madeAnyChange = true
          ChangeTo(changeTo)
        }
      }
      case e => DoChildren()
    }
  }

  def apply(e: Expr): Option[Expr] = {
    madeAnyChange = false
    val ne = visit_expr(this, e)
    val changed = madeAnyChange
    madeAnyChange = false
    if (changed) {
      Some(ne)
    } else {
      None
    }
  }

}

class Simplify(
    val res: Variable => Option[Expr],
    val initialBlock: Block = null,
    val absdom: Option[ConstCopyProp] = None /* flow sensitive */
) extends CILVisitor {

  var madeAnyChange = false
  var block: Block = initialBlock
  var skipped = Set[String]()

  override def vexpr(e: Expr) = {
    val threshold = 500
    val variables = e.variables.toSet
    val subst = Substitute(res, true, threshold)
    var old: Expr = e
    var result = subst(e).getOrElse(e)
    while (old != result) {
      old = result
      result = subst(e).getOrElse(e)
    }

    if (subst.complexity > threshold) {
      val bl = s"${block.parent.name}::${block.label}"
      if (!skipped.contains(bl)) {
        skipped = skipped + bl
        Logger.warn(s"Some skipped substitution at $bl due to resulting expr size > ${threshold} threshold")
      }
    }
    ChangeDoChildrenPost(result, x => x)
  }

  override def vblock(b: Block) = {
    block = b
    DoChildren()
  }

}
