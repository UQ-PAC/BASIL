package ir.transforms

import util.Logger
import ir.cilvisitor.*
import ir.*
import scala.collection.mutable
import analysis._

trait AbstractDomain[L] {
  def join(a: L, b: L): L
  def widen(a: L, b: L): L = join(a, b)
  def narrow(a: L, b: L): L = a
  def transfer(a: L, b: Statement): L

  def top: L
  def bot: L
}

def getRedundantAssignments(v: Procedure): Set[Assign] = {

  /** Get all assign statements which define a variable never used, assuming ssa form.
    */

  enum VS:
    case Bot
    case Assigned(definition: Set[Assign])
    case Read

  def joinVS(a: VS, b: VS) = {
    (a, b) match {
      case (VS.Bot, o)                        => o
      case (o, VS.Bot)                        => o
      case (VS.Read, _)                       => VS.Read
      case (_, VS.Read)                       => VS.Read
      case (VS.Assigned(d1), VS.Assigned(d2)) => VS.Assigned(d1 ++ d2)
    }
  }

  val assignedNotRead = mutable.Map[Variable, VS]().withDefaultValue(VS.Bot)

  for (c <- v) {
    c match {
      case a: Assign => {
        assignedNotRead(a.lhs) = joinVS(assignedNotRead(a.lhs), VS.Assigned(Set(a)))
        a.rhs.variables.foreach(v => {
          assignedNotRead(v) = VS.Read
        })
      }
      case m: MemoryAssign => {
        m.index.variables.foreach(v => {
          assignedNotRead(v) = VS.Read
        })
        m.value.variables.foreach(v => {
          assignedNotRead(v) = VS.Read
        })
      }
      case m: IndirectCall => {
        assignedNotRead(m.target) = VS.Read
      }
      case m: Assert => {
        m.body.variables.foreach(v => {
          assignedNotRead(v) = VS.Read
        })
      }
      case m: Assume => {
        m.body.variables.foreach(v => {
          assignedNotRead(v) = VS.Read
        })
      }
      case c: DirectCall => {
        c.actualParams
          .flatMap(_._2.variables)
          .foreach(v => {
            assignedNotRead(v) = VS.Read
          })

      }
      case p: Return => {
        p.outParams
          .flatMap(_._2.variables)
          .foreach(v => {
            assignedNotRead(v) = VS.Read
          })
      }
      case p: GoTo        => ()
      case p: NOP         => ()
      case p: Unreachable => ()
      case p: Procedure   => ()
      case b: Block       => ()
    }
  }

  assignedNotRead
    .collect { case (v, VS.Assigned(d)) =>
      d
    }
    .toSet
    .flatten
}

class CleanupAssignments extends CILVisitor {
  var redundant = Set[Assign]()

  override def vproc(p: Procedure) = {
    redundant = getRedundantAssignments(p).filter(_.rhs.loads.isEmpty)
    DoChildren()
  }

  override def vstmt(s: Statement) = s match {
    case a: Assign if redundant.contains(a) => ChangeTo(List())
    case _                                  => SkipChildren()
  }

}

def doCopyPropTransform(
    p: Program,
    reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])]
) = {

  var runs = 0
  var rerun = true
  while (runs < 5) {
    val rds2 = ReachingDefinitionsAnalysisSolver(p)
    val reachingDefs = rds2.analyze()
    val d = ConstCopyProp()
    applyRPO(p)

    // Logger.info(s"Simp run $runs")
    runs += 1
    val solver = worklistSolver(d)
    val result = solver.solveProg(p, Set(), Set())

    for ((p, xf) <- result) {
      val vis = Simplify(xf)
      visit_proc(vis, p)
      rerun = vis.madeAnyChange
    }
  }

  // cleanup
  // visit_prog(CleanupAssignments(), p)
  val toremove = p.collect {
    case b:Block if b.statements.size == 0 && b.prevBlocks.size == 1 && b.nextBlocks.size == 1 => b
  }
  for (b <- toremove) {
    val p = b.prevBlocks.head
    val n = b.nextBlocks.head
    p.replaceJump((GoTo(n)))
    b.parent.removeBlocks(b)
  }

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

  def solveProg(
      p: Program,
      widenpoints: Set[Block], // set of loop heads
      narrowpoints: Set[Block] // set of conditions
  ): Map[Procedure, L] = {
    val initDom = p.procedures.map(p =>
      (p, p.blocks.filter(x => (x.nextBlocks.size > 1 || x.prevBlocks.size > 1) && x.rpoOrder != -1))
    )

    initDom.map(d => (d._1, solve(d._2, Set(), Set()))).toMap
  }

  def solve(
      initial: IterableOnce[Block],
      widenpoints: Set[Block], // set of loop heads
      narrowpoints: Set[Block] // set of conditions
  ): L = {
    val saved: mutable.HashMap[Block, L] = mutable.HashMap()
    val worklist = mutable.PriorityQueue[Block]()(Ordering.by(b => b.rpoOrder))
    worklist.addAll(initial)

    var x = domain.bot
    while (worklist.nonEmpty) {
      val b = worklist.dequeue

      if (b.label == "lmain_goto_l00000321") {
        // println(s"$b\n  prevs : ${b.prevBlocks.map(c => saved.get(c))}")
      }

      while (worklist.nonEmpty && (worklist.head.rpoOrder >= b.rpoOrder)) do {
        // drop rest of blocks with same priority
        val m = worklist.dequeue()
        assert(
          m == b,
          s"Different nodes with same priority ${m.rpoOrder} ${b.rpoOrder}, violates PriorityQueueWorklist assumption: $b and $m"
        )
      }

      def bs(b: Block): List[Block] = {
        if (b.nextBlocks.size == 1) {
          val n = b.nextBlocks.head
          b :: bs(n)
        } else {
          List(b)
        }
      }

      x = b.prevBlocks.flatMap(ib => saved.get(ib).toList).foldLeft(x)(domain.join)
      val todo = bs(b)
      val lastBlock = todo.last

      def xf_block(x: L, b: Block) = b.statements.foldLeft(x)(domain.transfer)
      var nx = todo.foldLeft(x)(xf_block)
      saved(lastBlock) = nx
      if (nx != x) then {
        worklist.addAll(lastBlock.nextBlocks)
      }
      x = nx
    }
    x
  }
}

case class CopyProp(expr: Expr, deps: Set[Variable])

case class CCP(
    constants: Map[Variable, Literal],
    // variable -> expr * dependencies
    val exprs: Map[Variable, CopyProp]
)

class ConstCopyProp() extends AbstractDomain[CCP] {

  private final val callClobbers = (0 to 30).map("R" + _).map(c => Register(c, 64))

  def top: CCP = CCP(Map(), Map())
  def bot: CCP = CCP(Map(), Map())

  override def join(l: CCP, r: CCP): CCP = {
    CCP(
      (l.constants ++ r.constants)
        .removedAll(l.constants.keySet.intersect(r.constants.keySet).filterNot(k => l.constants(k) == r.constants(k))),
      (l.exprs ++ r.exprs).removedAll(l.exprs.keySet.intersect(r.exprs.keySet).filterNot(k => l.exprs(k) == r.exprs(k)))
    )
  }

  override def transfer(c: CCP, s: Statement): CCP = {
    s match {
      case m: MemoryAssign => {
        c.copy(exprs = c.exprs.filterNot((k, v) => v.expr.loads.nonEmpty))
      }
      case Assign(l, r, lb) => {
        var p = c
        val evaled = exprSimp(
          eval.partialEvalExpr(
            exprSimp(r),
            v => p.constants.get(v)
          )
        )
        val rhsDeps = evaled.variables

        p = evaled match {
          case lit: Literal => p.copy(constants = p.constants.updated(l, lit))
          case e: Expr => p.copy(constants = p.constants.removed(l), exprs = p.exprs.updated(l, CopyProp(e, e.variables)))
        }

        if (r.loads.nonEmpty) {
          // don't re-order loads
          p = p.copy(exprs = p.exprs.filter((k, v) => v.expr.loads.isEmpty))
        }
        // remove candidates whose value changes due to this update
        // without an SSA form in the output, we can't propagate assignments such that R0_1 := f(R0_0)
        //  or; only replace such that all uses are copyproped, the dead definition is removed
        p = p.copy(exprs = p.exprs.filterNot((k, v) => v.deps.exists(c => c.name == l.name)))

        p
      }
      case x: DirectCall => {
        val lhs = x.outParams.map(_._2)
        lhs.foldLeft(c)((c, l) => c.copy(constants = c.constants.removed(l), exprs = c.exprs.filterNot((k, v) => v.deps.contains(l)))
        )
      }
      case x: IndirectCall => {
        val toClob = callClobbers
        toClob.foldLeft(c)((c, l) =>
          c.copy(constants = c.constants.removed(l), exprs = c.exprs.filterNot((k, v) => v.deps.contains(l)))
        )
      }
      case _ => c
    }
  }
}


def exprSimp(e: Expr): Expr = {

  val assocOps: Set[BinOp] =
    Set(BVADD, BVMUL, BVOR, BVAND, BVEQ, BoolAND, BoolEQ, BoolOR, BoolEQUIV, BoolEQ, IntADD, IntMUL, IntEQ)

  def simpOne(e: Expr): Expr = {
    e match {
      // normalise
      case BinaryExpr(op, x: Literal, y: Expr) if !y.isInstanceOf[Literal] && assocOps.contains(op) => BinaryExpr(op, y, x)
      case BinaryExpr(BVADD, x: Expr, y: BitVecLiteral) if ir.eval.BitVectorEval.isNegative(y) => BinaryExpr(BVSUB, x, ir.eval.BitVectorEval.smt_bvneg(y))

      // identities
      case Extract(ed, 0, body) if (body.getType == BitVecType(ed))                        => exprSimp(body)
      case ZeroExtend(0, body)                                                             => exprSimp(body)
      case SignExtend(0, body)                                                             => exprSimp(body)
      case BinaryExpr(BVADD, body, BitVecLiteral(0, _))                                    => exprSimp(body)
      case BinaryExpr(BVMUL, body, BitVecLiteral(1, _))                                    => exprSimp(body)
      case Repeat(1, body)                                                                 => exprSimp(body)
      case Extract(ed, 0, ZeroExtend(extension, body)) if (body.getType == BitVecType(ed)) => exprSimp(body)
      case Extract(ed, 0, SignExtend(extension, body)) if (body.getType == BitVecType(ed)) => exprSimp(body)
      case BinaryExpr(BVXOR, l, r) if l == r =>
        e.getType match {
          case BitVecType(sz) => BitVecLiteral(0, sz)
        }
      // double negation
      case UnaryExpr(BVNOT, UnaryExpr(BVNOT, body))     => exprSimp(body)
      case UnaryExpr(BVNEG, UnaryExpr(BVNEG, body))     => exprSimp(body)
      case UnaryExpr(BoolNOT, UnaryExpr(BoolNOT, body)) => exprSimp(body)

      // compose slices
      case Extract(ed1, be1, Extract(ed2, be2, body)) => Extract(ed1 - be2, be1 + be2, exprSimp(body))

      // (comp (comp x y) 1) = (comp x y)
      case BinaryExpr(BVCOMP, body @ BinaryExpr(BVCOMP, _, _), BitVecLiteral(1, 1)) => exprSimp(body)
      case BinaryExpr(BVCOMP, body @ BinaryExpr(BVCOMP, _, _), BitVecLiteral(0, 1)) => UnaryExpr(BVNOT, exprSimp(body))
      case BinaryExpr(
            BVEQ,
            BinaryExpr(BVCOMP, body @ BinaryExpr(BVCOMP, _, _), BitVecLiteral(0, 1)),
            BitVecLiteral(1, 1)
          ) =>
        BinaryExpr(BVEQ, exprSimp(body), BitVecLiteral(0, 1))

      // constant folding
      // const + (e + const) -> (const + const) + e
      case BinaryExpr(BVADD, BinaryExpr(BVADD, body, l: BitVecLiteral), r: BitVecLiteral) =>
        BinaryExpr(BVADD, BinaryExpr(BVADD, l, r), exprSimp(body))
      case BinaryExpr(BVMUL, BinaryExpr(BVMUL, body, l: BitVecLiteral), r: BitVecLiteral) =>
        BinaryExpr(BVMUL, BinaryExpr(BVMUL, l, r), exprSimp(body))
      case BinaryExpr(BVOR, BinaryExpr(BVOR, body, l: BitVecLiteral), r: BitVecLiteral) =>
        BinaryExpr(BVOR, BinaryExpr(BVOR, l, r), exprSimp(body))
      case BinaryExpr(BVAND, BinaryExpr(BVAND, body, l: BitVecLiteral), r: BitVecLiteral) =>
        BinaryExpr(BVAND, BinaryExpr(BVAND, l, r), exprSimp(body))
      case r => r
    }
  }

  var pe = e
  var ne = simpOne(pe)
  while (ne != pe) {
    pe = ne
    ne = simpOne(pe)
  }
  ne
}

object SSARename:

// TODO: remove assigned and unread variables
// TODO: Make procedure calls and returns assignments of all registers so they can become global and be ssa,
//  and returned variables are not unread, and there is a clear modifies set across calls

  def concretiseSSA(
      p: Program,
      reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])]
  ) = {
    println("SSA conc ")
    //  println(reachingDefs)
    val c = SSACollect(p, reachingDefs)
    c.initial()
    visit_prog(c, p)
    // println(c.names)
    val rn = SSARename(p, c.names, reachingDefs)
    visit_prog(rn, p)
  }

  class SSARename(
      p: Program,
      renames: mutable.HashMap[Assign, Int],
      reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])]
  ) extends CILVisitor {
    var statement: Command = null

    override def vrvar(v: Variable) = {
      val assigns = statement match {
        case a: Assign => getUse(v, statement, reachingDefs)
        case _         => getDefinition(v, statement, reachingDefs)
      }
      assigns.headOption.map(doRename(v, _)) match {
        case Some(nv) => ChangeTo(nv)
        case None     => SkipChildren()
      }
    }

    def doRename(v: Variable, definition: Assign): Variable = {
      renames.get(definition) match {
        case Some(idx) => {
          v match {
            case Register(n, sz) => Register(n + "_" + idx, sz)
            case LocalVar(n, t)  => LocalVar(n + "_" + idx, t)
          }
        }
        case None => v
      }
    }

    override def vjump(s: Jump) = {
      statement = s
      DoChildren()
    }

    override def vstmt(s: Statement) = {
      statement = s
      s match {
        case a @ Assign(l, r, _) => {
          a.lhs = doRename(l, a)
          DoChildren()
        }
        case a: DirectCall => {
          // TODO: not handled
          // a.outParams = a.outParams.map(p => (p._1, doRename(p._2, a)
          DoChildren()
        }
        case _ => DoChildren()
      }
    }
  }

  def concretiseDSASinglePass(program: Program) = {

    applyRPO(program)
    case class DSARes(renames: Map[Variable, Int] = Map().withDefaultValue(-1)) // -1 means no rename

    def addIndex(v: Variable, idx: Int) = {
      if (idx != -1) {
        v match {
          case Register(n, sz) => Register(n + "_" + idx, sz)
          case LocalVar(n, t)  => LocalVar(n + "_" + idx, t)
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

    def njoin(
        st: Map[Block, DSARes],
        blocks: Iterable[Block],
        assignsAdd: mutable.Map[Block, Map[Int, Int]],
        count: mutable.Map[Variable, Int],
        lhss: Map[Command, Map[Variable, Int]],
        rhss: Map[Command, Map[Variable, Int]]
    ) = {
      var lhs = lhss
      var rhs = rhss
      var assignsappended: Map[Block, Map[Variable, Assign]] = Map().withDefaultValue(Map())
      require(blocks.size >= 2)
      val rs = blocks.map(st(_))
      val all = rs.flatMap(_.renames.keySet)
      val renames = all.collect {
        case v if rs.forall(rl => rs.foldLeft(true)((b, rr) => b && (rl.renames(v) == rr.renames(v)))) => {
          // all renames equal for this variable v
          v -> rs.head.renames(v)
        }
        case v => {
          // for branches which have different renamings of variables, we know the larger index renaming is so far unused
          // on the branches with smaller indexes, so we add a copy to these branches renaming to the largest index
          val maxrename = rs.map(_.renames(v)).foldLeft(-1)(Integer.max)
          for (b <- blocks) {
            if (st(b).renames(v) != maxrename && st(b).renames(v) != -1 && maxrename != -1) {
              // if there is a call on this block assigning the variable, update its outparam's ssa index
              // otherwise add an assignment to the block at the end or immediately before the call, and
              // update the ssa index of the in-parameters to the call
              b.statements.lastOption match {
                case Some(d: DirectCall) if d.outParams.toSet.map(_._2).contains(v) => {
                  rhs = rhs + (d -> (rhs.get(d).getOrElse(Map()) + (v -> st(b).renames(v))))
                }
                case c => {
                  val assign = assignsappended(b).get(v).getOrElse(Assign(v, v))
                  assignsappended = assignsappended + (b -> (assignsappended(b) + (v -> assign)))
                  lhs = lhs + (assign -> (lhs.get(assign).getOrElse(Map()) + (v -> maxrename)))
                  rhs = rhs + (assign -> (rhs.get(assign).getOrElse(Map()) + (v -> st(b).renames(v))))
                  c match {
                    case Some(call) => {
                      rhs = rhs + (call -> (rhs.get(call).getOrElse(Map()) + (v -> maxrename)))
                    }
                    case _ => ()
                  }
                }
              }
            }
          }
          v -> maxrename
        }
      }.toMap

      (DSARes(renames.withDefaultValue(-1)), lhs, rhs, assignsappended)
    }

    def join(
        st: Map[Block, DSARes],
        a: Block,
        b: Block,
        assignsAdd: mutable.Map[Block, Map[Int, Int]],
        count: mutable.Map[Variable, Int]
    ) = {
      val l = st(a)
      val r = st(b)

      val all = l.renames.keySet ++ r.renames.keySet
      val renames = all.collect {
        case v if (l.renames(v) != r.renames(v)) => {
          assignsAdd(a) = assignsAdd(a) + (l.renames(v) -> (count(v) + 1))
          assignsAdd(b) = assignsAdd(b) + (r.renames(v) -> (count(v) + 1))
          count(v) += 1
          v -> count(v)
        }
        case v /* if (l.renames(v) == r.renames(v)) */ => v -> l.renames(v)
      }.toMap

      DSARes(renames)
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

      for (s <- b.statements) {
        rh = rh.updated(s, rh(s) ++ r.renames)
        val renames: Map[Variable, Int] = s match {
          case a: Assign if (lh.get(a).flatMap(_.get(a.lhs))).map(_ == -1).getOrElse(true) => {
            count(a.lhs) = count(a.lhs) + 1
            Map(a.lhs -> count(a.lhs))
          }
          case a: DirectCall => {
            var nnr = mutable.Map[Variable, Int]()
            for (l <- a.outParams.map(_._2)) {
              if (lh.get(s).flatMap(_.get(l)).map(_ == -1).getOrElse(true)) {
                count(l) = count(l) + 1
                nnr += (l -> count(l))
              }
            }
            nnr.toMap
          }
          case _ => Map()
        }
        lh = lh.updated(s, lh(s) ++ renames)
        r = r.copy(renames = r.renames ++ rh(s) ++ lh(s) ++ renames)
      }
      r = r.copy(renames = r.renames ++ rh(b.jump))

      (r, lh, rh + (b.jump -> r.renames))
    }

    def renameAll(b: Block, lhs: Map[Command, Map[Variable, Int]], rhs: Map[Command, Map[Variable, Int]]) = {
      //println(s"${b.label}")
      for (s <- b.statements) {
        s match {
          case d: DirectCall => {
            //println(s"$s --- ${lhs.get(s)} ${rhs.get(s)}")
          }
          case _ => ()
        }
        visit_stmt(StmtRenamer(lhs.get(s).getOrElse(Map()), rhs.get(s).getOrElse(Map())), s)
      }
      val s = b.jump
      visit_jump(StmtRenamer(lhs.get(s).getOrElse(Map()), rhs.get(s).getOrElse(Map())), s)
    }

    def process(
        i: DSARes,
        b: Block,
        count: mutable.Map[Variable, Int]
    ): DSARes = {
      var r = i
      for (s <- b.statements) {
        val renames: Map[Variable, Int] = s match {
          case a: Assign => {
            count(a.lhs) = count(a.lhs) + 1
            Map(a.lhs -> count(a.lhs))
          }
          case a: DirectCall => {
            var nnr = mutable.Map[Variable, Int]()
            for (l <- a.outParams.map(_._2)) {
              count(l) = count(l) + 1
              nnr += (l -> count(l))
              // nnr.copy(renames = nnr.renames + (l -> count(l)))
            }
            nnr.toMap
          }
          case _ => Map()
        }
        visit_stmt(StmtRenamer(renames, r.renames), s)
        r = r.copy(renames = r.renames ++ renames)
      }
      visit_jump(StmtRenamer(Map(), r.renames), b.jump)
      r
    }

    def visitProc(p: Procedure) = {
      /*
       * visit once in weak topological order, collect renames and copies of ssa variables
       * we need to visit loops twice
       */

      // val worklist = mutable.PriorityQueue[Block]()(Ordering.by(b => b.rpoOrder))
      val worklist = mutable.Stack[Block]()
      worklist.pushAll(p.entryBlock)
      var seen = Set[Block]()
      val assignsAdd = mutable.Map[Block, Map[Int, Int]]()
      val count = mutable.Map[Variable, Int]().withDefaultValue(0)
      var lhs = Map[Command, Map[Variable, Int]]().withDefaultValue(Map().withDefaultValue(-1))
      var rhs = Map[Command, Map[Variable, Int]]().withDefaultValue(Map().withDefaultValue(-1))

      // val stmtRenames = mutable.Map[Command, Map[Variable, Int]]()

      var st = Map[Block, DSARes]().withDefaultValue(DSARes())
      val bprocessed = mutable.Set[Block]()
      var assignsappended: Map[Block, Map[Variable, Assign]] = Map().withDefaultValue(Map())

      while (worklist.nonEmpty) {
        val b = worklist.pop

        //println(b)

        if (b.prevBlocks.forall(st.contains(_)) && !seen(b)) {
          worklist.pushAll(b.prevBlocks)
          worklist.push(b)
        } else {
          val prev = if (b.incomingJumps.size > 1 && b.prevBlocks.forall(b => st.contains(b))) {
            val (n, llhs, rrhs, assigns) = njoin(st, b.prevBlocks, assignsAdd, count, lhs, rhs)
            assignsappended = assignsappended ++ assigns
            lhs = llhs
            rhs = rrhs
            n
          } else if (b.incomingJumps.size == 1) {
            st(b.incomingJumps.head.parent)
          } else {
            DSARes()
          }
          val (processed, nlhs, nrhs) = processCollect(prev, b, count, lhs, rhs)
          if (st(b) != processed || lhs != lhs || rhs != rhs) {
            lhs = nlhs
            rhs = nrhs
            //println(processed)
            //println()
            worklist.pushAll(b.nextBlocks)
            st = st.updated(b, processed)
          }
        }
        //println(worklist.size)
        seen += b
      }
      for (b <- assignsappended) {
        for (v <- b._2) {
          appendAssign(b._1, v._2)
        }
      }

      for (b <- p.blocks) {
        renameAll(b, lhs, rhs)
      }
      //println(s"done ${p.name}")

    }

    program.procedures.foreach(visitProc)
    //println(s"done dsa}")
  }

  class SSASimp(p: Program) extends CILVisitor {

    val names = mutable.HashMap[Command, Map[Variable, Int]]()
    val copies = mutable.Map[Block, List[Statement]]()

    var count = mutable.HashMap[Variable, Int]().withDefaultValue(0)
    var statement: Statement = null

    def rename(rv: Variable, oldIndex: Int, newIndex: Int) = {
      names.foreach(n => {
        names(n._1) = {
          n._2.map(v =>
            v match {
              case (v, i) if v == rv && i == oldIndex => (v, newIndex)
              case o                                  => o
            }
          )
        }
      })
    }

    def initial() = {
      // give each definition a unique index
      for (c <- p) {
        c match {
          case b: Block if (b.incomingJumps.size > 1) => {}
          case c: Command =>
            c match {
              case a: Assign => {
                count(a.lhs) = count(a.lhs) + 1
                names(a) = names(a) + (a.lhs -> count(a.lhs))
              }
              case a: DirectCall => {
                for (l <- a.outParams.map(_._2)) {
                  count(l) = count(l) + 1
                  names(a) = names(a) + (l -> count(l))
                }
              }
              case _ => ()
            }
          case _ => ()
        }
      }
    }

  }

  class SSACollect(p: Program, reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])])
      extends CILVisitor {
    val names = mutable.HashMap[Assign, Int]()
    var count = mutable.HashMap[Variable, Int]().withDefaultValue(0)
    var statement: Statement = null

    override def vrvar(v: Variable) = {
      val assigns = getUse(v, statement, reachingDefs)
      if (assigns.size > 1) {
        count(v) = count(v) + 1
        for (a <- assigns) {
          names(a) = count(v)
        }
      }
      DoChildren()
    }

    def initial() = {
      // give each definition a unique index
      for (c <- p) {
        c match {
          case a: Assign => {
            count(a.lhs) = count(a.lhs) + 1
            names(a) = count(a.lhs)
          }
          case _ => ()
        }
      }
    }

    override def vstmt(s: Statement) = {
      statement = s
      DoChildren()
    }

  }

class Simplify(
    val res: CCP
) extends CILVisitor {

  var madeAnyChange = false
  var statement: Statement = null

  def simp(pe: Expr)(ne: Expr) = {
    val simped = eval.partialEvalExpr(exprSimp(ne), x => None)
    if (pe != simped) {
      madeAnyChange = true
    }

    simped
  }

  override def vexpr(e: Expr) = {
    e match {
      case v: Variable if res.constants.contains(v) => {
        madeAnyChange = true
        ChangeDoChildrenPost(
          res.constants(v),
          simp(e)
        )
      }
      case v: Variable if res.exprs.contains(v) => {
        val repl = res.exprs(v)
        madeAnyChange = true
        ChangeDoChildrenPost(
          repl.expr,
          simp(e)
        )
      }
      case e => ChangeDoChildrenPost(e, simp(e))
    }
  }

  override def vstmt(s: Statement) = {
    statement = s

    s match {
      case a @ Assign(l, r, lb) => {
        val state = res
        val nr = eval.partialEvalExpr(
          r,
          v => state.constants.get(v)
        )
        if (nr != r) {
          madeAnyChange = true
        }
        a.rhs = nr
        DoChildren()
      }
      case _ => DoChildren()
    }
  }
}
