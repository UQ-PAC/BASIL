
package ir

import scala.collection.mutable
import scala.collection.immutable.*
import org.scalatest.funsuite.AnyFunSuite
import util.intrusive_list.*
import ir.dsl.*
import ir.cilvisitor.*


class CILVisTest extends AnyFunSuite {

  def getRegister(name: String) = Register(name, 64)
  val program: Program = prog(
    proc("main",
      block("0x0",
        Assign(getRegister("R6"), getRegister("R31")),
        goto("0x1")
      ),
      block("0x1",
        MemoryAssign(mem, BinaryExpr(BVADD, getRegister("R6"), bv64(4)), bv64(10), Endian.LittleEndian, 64),
        goto("returntarget")
      ),
      block("returntarget",
        ret
      )
    )
  )
  test("trace prog") {
    val p = prog(
      proc("main",
        block("lmain",
          goto("lmain1")
        ),
        block("lmain1",
          goto("lmain2")),
        block("lmain2",
          ret)
      )
    )

    class BlockTrace extends CILVisitorImpl {
      val res = mutable.ArrayBuffer[String]()

      override def vblock(b: Block) = {
        res.append(b.label)
        VisitAction.DoChildren()
      }

      override def vjump(b: Jump) = {
        b match {
          case g: GoTo => res.addAll(g.targets.map(t => s"gt_${t.label}").toList)
          case r: IndirectCall => res.append("indirect")
          case r: DirectCall => res.append("direct")
        }
        VisitAction.DoChildren()
      }
    }

    val v = BlockTrace()
    visit_proc(v, p.procedures.head)
    assert(v.res.toList == List("lmain", "gt_lmain1", "lmain1", "gt_lmain2",  "lmain2", "indirect"))
  }



  test("visit exprs") {


    class ExprTrace extends CILVisitorImpl {
      val res = mutable.ArrayBuffer[String]()

      override def vlvar(e: Variable) = {
        e match {
          case Register(n, _) => res.append(n); 
          case _ => ???
        }
        VisitAction.DoChildren()
      }

      override def vexpr(e: Expr) = {
        e match {
          case BinaryExpr(op, l, r) => res.append(op.toString)
          case Register(n, _) => res.append(n.toString)
          case n: Literal => res.append(n.toString)
          case _ => ()
        }
        VisitAction.DoChildren()
      }
    }

    val v = ExprTrace()
    visit_proc(v, program.procedures.head)
    assert(v.res.toList == List("R31", "R6", "add", "R6", "4bv64", "10bv64"))
  }

  test("rewrite exprs") {

    class VarTrace extends CILVisitorImpl {
      val res = mutable.ArrayBuffer[String]()

      override def vlvar(e: Variable) = {res.append(e.name); VisitAction.SkipChildren()}
      override def vvar(e: Variable) = {res.append(e.name); VisitAction.SkipChildren()}

    }


    class RegReplace extends CILVisitorImpl {
      val res = mutable.ArrayBuffer[String]()

      override def vlvar(e: Variable) = {
        e match {
          case Register(n, sz) => VisitAction.ChangeTo(LocalVar("l" + n, e.getType)); 
          case _ => VisitAction.DoChildren()
        }
      }

      override def vexpr(e: Expr) = {
        e match {
          case Register(n, _) => VisitAction.ChangeTo(LocalVar("l" + n, e.getType)); 
          case _ => VisitAction.DoChildren()
        }
      }
    }

    class RegReplacePost extends CILVisitorImpl {
      val res = mutable.ArrayBuffer[String]()

      override def vlvar(e: Variable) = {
        e match {
          case LocalVar(n,_) => VisitAction.ChangeDoChildrenPost(LocalVar("e" + n, e.getType), e => {res.append(e.name); e}); 
          case _ => VisitAction.DoChildren()
        }
      }

      override def vexpr(e: Expr) = {
        e match {
          case LocalVar(n, _) => VisitAction.ChangeDoChildrenPost(LocalVar("e" + n, e.getType), e => {
            res.append(e match {
            case v: Variable => v.name
            case _ => ??? 
            }); e}); 
          case _ => VisitAction.DoChildren()
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


}
