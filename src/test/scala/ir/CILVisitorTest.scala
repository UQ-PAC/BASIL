package ir

import scala.collection.mutable
import scala.collection.immutable.*
import org.scalatest.funsuite.AnyFunSuite
import ir.dsl.*
import ir.cilvisitor.*
import test_util.CaptureOutput

class FindVars extends CILVisitor {
  val vars = mutable.ArrayBuffer[Variable]()

  override def vrvar(v: Variable) = {
    vars.append(v)
    SkipChildren()
  }
  override def vlvar(v: Variable) = {
    vars.append(v)
    SkipChildren()
  }

  def globals = vars.collect {
    case g: Global =>
      g
  }
}

def globals(e: Expr): List[Variable] = {
  val v = FindVars()
  visit_expr(v, e)
  v.globals.toList
}

def gamma_v(l: Variable) = LocalVar("Gamma_" + l.name, BoolType)

def gamma_e(e: Expr): Expr = {
  globals(e) match {
    case Nil => TrueLiteral
    case hd :: Nil => hd
    case hd :: tl => tl.foldLeft(hd: Expr)((l, r) => BinaryExpr(BoolAND, l, gamma_v(r)))
  }
}

class AddGammas extends CILVisitor {

  override def vstmt(s: Statement) = {
    s match {
      case a: LocalAssign => ChangeTo(List(a, LocalAssign(gamma_v(a.lhs), gamma_e(a.rhs))))
      case _ => SkipChildren()
    }

  }
}

@test_util.tags.UnitTest
class CILVisitorTest extends AnyFunSuite with CaptureOutput {

  def getRegister(name: String) = Register(name, 64)
  test("trace prog") {
    val p = prog(proc("main", block("lmain", goto("lmain1")), block("lmain1", goto("lmain2")), block("lmain2", ret)))

    class BlockTrace extends CILVisitor {
      val res = mutable.ArrayBuffer[String]()

      override def vblock(b: Block) = {
        res.append(b.label)
        DoChildren()
      }

      override def vjump(b: Jump) = {
        b match {
          case g: GoTo => res.addAll(g.targets.map(t => s"gt_${t.label}").toList)
          case _: Return => res.append("return")
          case _: Unreachable => res.append("direct")
        }
        DoChildren()
      }
    }

    val v = BlockTrace()
    visit_proc(v, p.procedures.head)
    assert(v.res.toList == List("lmain", "gt_lmain1", "lmain1", "gt_lmain2", "lmain2", "return"))
  }

  test("visit exprs") {
    val program: Program = prog(
      proc(
        "main",
        block("0x0", LocalAssign(getRegister("R6"), getRegister("R31")), goto("0x1")),
        block(
          "0x1",
          MemoryStore(mem, BinaryExpr(BVADD, getRegister("R6"), bv64(4)), bv64(10), Endian.LittleEndian, 64),
          goto("returntarget")
        ),
        block("returntarget", ret)
      )
    )

    class ExprTrace extends CILVisitor {
      val res = mutable.ArrayBuffer[String]()

      override def vlvar(e: Variable) = {
        e match {
          case Register(n, _) => res.append(n);
          case _ => ??? // only reg in source program
        }
        DoChildren()
      }
      override def vrvar(e: Variable) = {
        e match {
          case Register(n, _) => res.append(n);
          case _ => ??? // only reg in source program
        }
        DoChildren()
      }

      override def vexpr(e: Expr) = {
        e match {
          case BinaryExpr(op, _, _) => res.append(op.toString)
          case n: Literal => res.append(n.toString)
          case _ => ()
        }
        DoChildren()
      }
    }

    val v = ExprTrace()
    visit_proc(v, program.procedures.head)
    assert(v.res.toList == List("R31", "R6", "add", "R6", "4bv64", "10bv64"))
  }

  test("rewrite exprs") {

    val program: Program = prog(
      proc(
        "main",
        block("0x0", LocalAssign(getRegister("R6"), getRegister("R31")), goto("0x1")),
        block(
          "0x1",
          MemoryStore(mem, BinaryExpr(BVADD, getRegister("R6"), bv64(4)), bv64(10), Endian.LittleEndian, 64),
          goto("returntarget")
        ),
        block("returntarget", ret)
      )
    )
    class VarTrace extends CILVisitor {
      val res = mutable.ArrayBuffer[String]()

      override def vrvar(e: Variable) = { res.append(e.name); SkipChildren() }
      override def vlvar(e: Variable) = { res.append(e.name); SkipChildren() }

    }

    class RegReplace extends CILVisitor {
      override def vrvar(e: Variable) = {
        e match {
          case Register(n, _) => ChangeTo(LocalVar("l" + n, e.getType));
          case _ => DoChildren()
        }
      }
      override def vlvar(e: Variable) = {
        e match {
          case Register(n, _) => ChangeTo(LocalVar("l" + n, e.getType));
          case _ => DoChildren()
        }
      }

    }

    class RegReplacePost extends CILVisitor {
      val res = mutable.ArrayBuffer[String]()
      override def vlvar(e: Variable) = {
        e match {
          case LocalVar(n, _, _) =>
            ChangeDoChildrenPost(LocalVar("e" + n, e.getType), e => { res.append(e.name); e });
          case _ => DoChildren()
        }
      }

      override def vrvar(e: Variable) = {
        e match {
          case LocalVar(n, _, _) =>
            ChangeDoChildrenPost(LocalVar("e" + n, e.getType), e => { res.append(e.name); e });
          case _ => DoChildren()
        }
      }

    }

    val v = VarTrace()
    visit_proc(v, program.procedures.head)
    assert(v.res.toList == List("R31", "R6", "R6"))
    visit_proc(RegReplace(), program.procedures.head)
    val v2 = VarTrace()
    visit_proc(v2, program.procedures.head)
    assert(v2.res.toList == List("lR31", "lR6", "lR6"))

    val v3 = RegReplacePost()
    visit_proc(v3, program.procedures.head)
    assert(v3.res.toList == List("elR31", "elR6", "elR6"))

  }

  test("changedochildrenposttest") {

    val expr = BinaryExpr(
      BVADD,
      BitVecLiteral(BigInt(12), 32),
      (BinaryExpr(BVADD, BitVecLiteral(BigInt(100), 32), BitVecLiteral(BigInt(120), 32)))
    )
    class vis extends CILVisitor {

      override def vexpr(e: Expr) = {
        ChangeDoChildrenPost(
          e match {
            case BitVecLiteral(100, 32) => BitVecLiteral(111, 32)
            case _ => e
          },
          x =>
            x match {
              case BitVecLiteral(111, 32) => LocalVar("beans", BitVecType(32))
              case _ => x
            }
        )
      }
    }

    val cexpr = BinaryExpr(
      BVADD,
      BitVecLiteral(BigInt(12), 32),
      (BinaryExpr(BVADD, LocalVar("beans", BitVecType(32)), BitVecLiteral(BigInt(120), 32)))
    )

    val ne = visit_expr(vis(), expr)
    assert(ne == cexpr)

  }

}
