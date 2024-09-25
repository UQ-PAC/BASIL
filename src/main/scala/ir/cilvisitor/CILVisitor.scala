package ir.cilvisitor

import ir.*
import scala.collection.mutable.ArrayBuffer

/** A new visitor based off CIL.
  *
  * Copied from ASLi https://github.com/UQ-PAC/aslp/blob/partial_eval/libASL/asl_visitor.ml copying from George Necula's
  * CIL project (https://people.eecs.berkeley.edu/~necula/cil/)
  */

sealed trait VisitAction[T]
case class SkipChildren[T]() extends VisitAction[T]
case class DoChildren[T]() extends VisitAction[T]
case class ChangeTo[T](e: T) extends VisitAction[T]
// changes to e, then visits children of e, then applies f to the result
case class ChangeDoChildrenPost[T](e: T, f: T => T) extends VisitAction[T]


trait CILVisitor:
  def vprog(e: Program): VisitAction[Program] = DoChildren()
  def vproc(e: Procedure): VisitAction[List[Procedure]] = DoChildren()
  def vparams(e: ArrayBuffer[Parameter]): VisitAction[ArrayBuffer[Parameter]] = DoChildren()
  def vblock(e: Block): VisitAction[Block] = DoChildren()

  def vstmt(e: Statement): VisitAction[List[Statement]] = DoChildren()
  def vjump(j: Jump): VisitAction[Jump] = DoChildren()
  def vfallthrough(j: Option[GoTo]): VisitAction[Option[GoTo]] = DoChildren()

  def vexpr(e: Expr): VisitAction[Expr] = DoChildren()
  def vvar(e: Variable): VisitAction[Variable] = DoChildren()
  def vlvar(e: Variable): VisitAction[Variable] = DoChildren()
  def vmem(e: Memory): VisitAction[Memory] = DoChildren()

  def enter_scope(params: ArrayBuffer[Parameter]): Unit = ()
  def leave_scope(outparam: ArrayBuffer[Parameter]): Unit = ()


def doVisitList[T](v: CILVisitor, a: VisitAction[List[T]], n: T, continue: (T) => T): List[T] = {
  a match {
    case SkipChildren()             => List(n)
    case ChangeTo(z)                => z
    case DoChildren()               => List(continue(n))
    case ChangeDoChildrenPost(x, f) => f(x.map(continue(_)))
  }
}

def doVisit[T](v: CILVisitor, a: VisitAction[T], n: T, continue: (T) => T): T = {
  a match {
    case SkipChildren()             => n
    case DoChildren()               => continue(n)
    case ChangeTo(z)                => z
    case ChangeDoChildrenPost(x, f) => f(continue(x))
  }
}

class CILVisitorImpl(val v: CILVisitor) {

  def visit_parameters(p: ArrayBuffer[Parameter]): ArrayBuffer[Parameter] = {
    doVisit(v, v.vparams(p), p, (n) => n)
  }

  def visit_var(n: Variable): Variable = {
    doVisit(v, v.vvar(n), n, (n) => n)
  }

  def visit_lvar(n: Variable): Variable = {
    doVisit(v, v.vlvar(n), n, (n) => n)
  }

  def visit_mem(n: Memory): Memory = {
    doVisit(v, v.vmem(n), n, (n) => n)
  }


  def visit_jump(j: Jump): Jump = {
    doVisit(v, v.vjump(j), j, (j) => j)
  }

  def visit_fallthrough(j: Option[GoTo]): Option[GoTo] = {
    doVisit(v, v.vfallthrough(j), j, (j) => j)
  }

  def visit_expr(n: Expr): Expr = {
    def continue(n: Expr): Expr = n match {
      case n: Literal                           => n
      case MemoryLoad(mem, index, endian, size) => MemoryLoad(visit_mem(mem), visit_expr(index), endian, size)
      case Extract(end, start, arg)             => Extract(end, start, visit_expr(arg))
      case Repeat(repeats, arg)                 => Repeat(repeats, visit_expr(arg))
      case ZeroExtend(bits, arg)                => ZeroExtend(bits, visit_expr(arg))
      case SignExtend(bits, arg)                => SignExtend(bits, visit_expr(arg))
      case BinaryExpr(op, arg, arg2)            => BinaryExpr(op, visit_expr(arg), visit_expr(arg2))
      case UnaryExpr(op, arg)                   => UnaryExpr(op, visit_expr(arg))
      case v: Variable                          => visit_var(v)
      case UninterpretedFunction(n, params, rt) => UninterpretedFunction(n, params.map(visit_expr), rt)
    }
    doVisit(v, v.vexpr(n), n, continue)
  }

  def visit_stmt(s: Statement): List[Statement] = {
    def continue(n: Statement) = n match {
      case d: DirectCall => d
      case i: IndirectCall => {
        i.target = visit_var(i.target)
        i
      }
      case m: MemoryAssign => {
        m.mem = visit_mem(m.mem)
        m.index = visit_expr(m.index)
        m.value = visit_expr(m.value)
        m
      }
      case m: Assign => {
        m.rhs = visit_expr(m.rhs)
        m.lhs = visit_lvar(m.lhs)
        m
      }
      case s: Assert => {
        s.body = visit_expr(s.body)
        s
      }
      case s: Assume => {
        s.body = visit_expr(s.body)
        s
      }
      case n: NOP => n
    }
    doVisitList(v, v.vstmt(s), s, continue)
  }

  def visit_block(b: Block): Block = {
    def continue(b: Block) = {
      b.statements.foreach(s => {
        val r = visit_stmt(s)
        r match {
          case Nil => b.statements.remove(s)
          case n :: tl =>
            b.statements.replace(s, n)
            b.statements.insertAllAfter(Some(n), tl)
        }
      })
      b.replaceJump(visit_jump(b.jump))
      b
    }

    doVisit(v, v.vblock(b), b, continue)
  }

  def visit_proc(p: Procedure): List[Procedure] = {
    def continue(p: Procedure) = {
      p.in = visit_parameters(p.in)
      v.enter_scope(p.in)
      for (b <- p.blocks) {
        p.replaceBlock(b, visit_block(b))
      }
      p.out = visit_parameters(p.out)
      v.leave_scope(p.out)
      p
    }

    doVisitList(v, v.vproc(p), p, continue)
  }

  def visit_prog(p: Program): Program = {
    def continue(p: Program) = {
      p.procedures = p.procedures.flatMap(visit_proc)
      p
    }
    doVisit(v, v.vprog(p), p, continue)
  }
}

def visit_block(v: CILVisitor, b: Block): Block = CILVisitorImpl(v).visit_block(b)
def visit_proc(v: CILVisitor, b: Procedure): List[Procedure] = CILVisitorImpl(v).visit_proc(b)
def visit_prog(v: CILVisitor, b: Program): Program = CILVisitorImpl(v).visit_prog(b)
def visit_stmt(v: CILVisitor, e: Statement): List[Statement] = CILVisitorImpl(v).visit_stmt(e)
def visit_jump(v: CILVisitor, e: Jump): Jump = CILVisitorImpl(v).visit_jump(e)
def visit_expr(v: CILVisitor, e: Expr): Expr = CILVisitorImpl(v).visit_expr(e)
