package ir.cilvisitor

import ir.*
import scala.collection.mutable.ArrayBuffer

/** A new visitor based off CIL.
  *
  * Copied from ASLi https://github.com/UQ-PAC/aslp/blob/partial_eval/libASL/asl_visitor.ml copying from George Necula's
  * CIL project (https://people.eecs.berkeley.edu/~necula/cil/)
  */

enum VisitAction[T]:
  case SkipChildren()
  case DoChildren()
  case ChangeTo(e: T)
  // changes to e, then visits children of e, then applies f to the result
  case ChangeDoChildrenPost(e: T, f: T => T)

import VisitAction._

trait CILVisitorImpl:
  def vprog(e: Program): VisitAction[Program] = VisitAction.DoChildren()
  def vproc(e: Procedure): VisitAction[List[Procedure]] = VisitAction.DoChildren()
  def vparams(e: ArrayBuffer[Parameter]): VisitAction[ArrayBuffer[Parameter]] = VisitAction.DoChildren()
  def vblock(e: Block): VisitAction[Block] = VisitAction.DoChildren()

  def vstmt(e: Statement): VisitAction[List[Statement]] = VisitAction.DoChildren()
  def vjump(j: Jump): VisitAction[Jump] = VisitAction.DoChildren()
  def vfallthrough(j: Option[GoTo]): VisitAction[Option[GoTo]] = VisitAction.DoChildren()

  def vexpr(e: Expr): VisitAction[Expr] = VisitAction.DoChildren()
  // right-expressions
  def vvar(e: Variable): VisitAction[Variable] = VisitAction.DoChildren()
  def vmem(e: Memory): VisitAction[Memory] = VisitAction.DoChildren()
  def vload(e: MemoryLoad): VisitAction[MemoryLoad] = VisitAction.DoChildren()

  // left-expressions
  // Somewhat confusing that these do not get visited by vexpr
  def vlvar(e: Variable): VisitAction[Variable] = VisitAction.DoChildren()
  def vlmem(e: Memory): VisitAction[Memory] = VisitAction.DoChildren()

  def enter_scope(params: ArrayBuffer[Parameter]): Unit = ()
  def leave_scope(outparam: ArrayBuffer[Parameter]) : Unit = ()


class CILVisitor(val v: CILVisitorImpl) {

  def doVisitList[T](v: CILVisitorImpl, a: VisitAction[List[T]], n: T, continue: (CILVisitorImpl, T) => T): List[T] = {
    a match {
      case VisitAction.SkipChildren()             => List(n)
      case VisitAction.ChangeTo(z)                => z
      case VisitAction.DoChildren()               => List(continue(v, n))
      case VisitAction.ChangeDoChildrenPost(x, f) => f(x.map(continue(v, _)))
    }
  }

  def doVisit[T](v: CILVisitorImpl, a: VisitAction[T], n: T, continue: (CILVisitorImpl, T) => T): T = {
    a match {
      case VisitAction.SkipChildren()             => n
      case VisitAction.DoChildren()               => continue(v, n)
      case VisitAction.ChangeTo(z)                => z
      case VisitAction.ChangeDoChildrenPost(x, f) => f(continue(v, x))
    }
  }

  def visit_parameters(p: ArrayBuffer[Parameter]): ArrayBuffer[Parameter] = {
    doVisit(v, v.vparams(p), p, (_, n) => n)
  }

  def visit_load(n: MemoryLoad) = {
    def continue(v: CILVisitorImpl, l: MemoryLoad): MemoryLoad = {
      MemoryLoad(visit_mem(l.mem), visit_expr(l.index), l.endian, l.size)
    }
    doVisit(v, v.vload(n), n, continue)
  }

  def visit_var(n: Variable): Variable = {
    doVisit(v, v.vvar(n), n, (_, n) => n)
  }

  def visit_lvar(n: Variable): Variable = {
    doVisit(v, v.vlvar(n), n, (_, n) => n)
  }

  def visit_mem(n: Memory): Memory = {
    doVisit(v, v.vmem(n), n, (_, n) => n)
  }

  def visit_lmem(n: Memory): Memory = {
    doVisit(v, v.vlmem(n), n, (_, n) => n)
  }

  def visit_jump(j: Jump): Jump = {
    doVisit(v, v.vjump(j), j, (_, j) => j)
  }

  def visit_fallthrough(j: Option[GoTo]): Option[GoTo] = {
    doVisit(v, v.vfallthrough(j), j, (_, j) => j)
  }

  def visit_expr(n: Expr): Expr = {
    def continue(v: CILVisitorImpl, n: Expr): Expr = n match {
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
    def continue(v: CILVisitorImpl, n: Statement) = n match {
      case m: MemoryAssign => {
        m.mem = visit_lmem(m.mem)
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
    def continue(v: CILVisitorImpl, b: Block) = {
      b.statements.foreach(s => {
        val r = visit_stmt(s)
        r match {
          case Nil      => b.statements.remove(s)
          case n :: Nil => b.statements.replace(s, n)
          case n :: tl => {
            b.statements.replace(s, n)
            var p = n
            for (i <- tl) {
              b.statements.insertAfter(p, i)
              p = i
            }
          }
        }
      })
      b.replaceJump(visit_jump(b.jump))
      b.fallthrough = visit_fallthrough(b.fallthrough)
      b
    }

    doVisit(v, v.vblock(b), b, continue)
  }

  def visit_proc(p: Procedure): List[Procedure] = {
    def continue(v: CILVisitorImpl, p: Procedure) = {
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

  def visit_proc(p: Program): Program = {
    def continue(v: CILVisitorImpl, p: Program) = {
      p.procedures = p.procedures.flatMap(visit_proc)
      p
    }
    doVisit(v, v.vprog(p), p, continue)
  }
}

def visit_block(v: CILVisitorImpl, b: Block): Block = CILVisitor(v).visit_block(b)
def visit_proc(v: CILVisitorImpl, b: Procedure): List[Procedure] = CILVisitor(v).visit_proc(b)
def visit_stmt(v: CILVisitorImpl, e: Statement): List[Statement] = CILVisitor(v).visit_stmt(e)
def visit_jump(v: CILVisitorImpl, e: Jump): Jump = CILVisitor(v).visit_jump(e)
def visit_expr(v: CILVisitorImpl, e: Expr): Expr = CILVisitor(v).visit_expr(e)
