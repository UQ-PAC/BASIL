package ir.transforms

import ir.cilvisitor.*
import ir.eval.*
import ir.{IRContext, *}
import util.functional.*

def liftLinuxAssertFail(ctx: IRContext) = {

  /** https://refspecs.linuxfoundation.org/LSB_1.3.0/gLSB/gLSB/baselib---assert-fail-1.html
    */

  val begin = InterpFuns.initProgState(NormalInterpreter)(ctx, InterpreterState())
  def getString(x: BitVecLiteral): Option[String] = {
    val o = Eval.getNullTerminatedString(NormalInterpreter)("mem", Scalar(x))
    def bvToChar(b: BitVecLiteral) = b match {
      case BitVecLiteral(i, 8) => i.toChar
      case _ => ???
    }
    State.evaluate(begin, o) match {
      case Left(e) => None
      case Right(xs) => Some(xs.map(bvToChar).mkString(""))
    }
  }

  val assertParam = LocalVar("R0_in", BitVecType(64))
  val fileParam = LocalVar("R1_in", BitVecType(64))
  val lineNo = LocalVar("R2_in", BitVecType(64))
  val funNameParam = LocalVar("R3_in", BitVecType(64))

  sealed trait Replace
  case class AssertFail(info: Option[String], filename: Option[String], lineNo: Option[Int], function: Option[String])
      extends Replace
  case class Abort() extends Replace

  def getBV(e: Expr) = e match {
    case b: BitVecLiteral => Some(b)
    case _ => None
  }

  val asserts: Map[DirectCall, Replace] = ctx.program.collect {
    case d: DirectCall if d.target.procName == "__assert_fail" => {
      d -> AssertFail(
        None,
        None,
        None,
        None
        // getBV(d.actualParams(assertParam)).flatMap(getString),
        // getBV(d.actualParams(fileParam)).flatMap(getString),
        // getBV(d.actualParams(lineNo)).map(_.value.toInt),
        // getBV(d.actualParams(funNameParam)).flatMap(getString)
      )
    }
    case d: DirectCall if d.target.procName == "abort" => {
      d -> Abort()
    }
  }.toMap

  class Replacer extends CILVisitor {

    override def vstmt(s: Statement) = {
      s match {
        case d: DirectCall if asserts.contains(d) => {
          asserts(d) match {
            case af: AssertFail => {
              val msg = af.info.getOrElse("")
              val line = af.filename.map(fn => s" $fn:${af.lineNo.map(_.toString).getOrElse("?")}").getOrElse("")
              val fun = af.function.map(f => s" @ $f").getOrElse("")
              ChangeTo(List(Assert(FalseLiteral, Some(s"call __assert_fail $msg$fun$line"))))
            }
            case a: Abort => {
              ChangeTo(List(Assert(FalseLiteral, Some("abort"))))
            }
          }
        }
        case _ => SkipChildren()
      }
    }
  }

  visit_prog(Replacer(), ctx.program)
}

def liftSVCompNonDetEarlyIR(p: Program) = {

  /*
   * Run after parameter form
   */

  def nonDetFunc(name: String, size: Int) = FApplyExpr(name, Seq(), BitVecType(size))

  /*
   * Officially possible:
   *
   * bool, char, int, float, double, loff_t, long, pchar, pthread_t, sector_t, short, size_t, u32, uchar, uint, ulong, unsigned, ushort
   */
  val typeToSize =
    Map("ulong" -> 64, "long" -> 64, "bool" -> 8, "short" -> 8, "char" -> 8, "uchar" -> 8, "int" -> 32, "uint" -> 32)
      .withDefaultValue(64)

  class ReplaceNondet extends CILVisitor {

    override def vstmt(s: Statement) = s match {
      case d: DirectCall if d.target.name.startsWith("__VERIFIER_nondet") => {
        val lhs = Register("R0", 64)
        val oSize = 64

        val ndtype = d.target.procName.stripPrefix("__VERIFIER_nondet_")

        val size = typeToSize(ndtype)
        val ns = if (oSize > size) {
          LocalAssign(lhs, ZeroExtend(oSize - size, nonDetFunc(d.target.procName, size)))
        } else if (oSize == size) {
          LocalAssign(lhs, nonDetFunc(d.target.procName, size))
        } else {
          throw Exception("unreachable")
        }

        ChangeTo(List(ns))
      }
      case _ => SkipChildren()
    }
  }

  visit_prog(ReplaceNondet(), p)

}

def liftSVComp(p: Program) = {
  /*
   * Run after parameter form
   */

  class ReplaceNondet extends CILVisitor {

    override def vstmt(s: Statement) = s match {
      case d: DirectCall if d.target.procName == "__VERIFIER_error" => {
        ChangeTo(List(Assert(FalseLiteral, Some("__VERIFIER_error"))))
      }
      case d: DirectCall if d.target.procName == "__VERIFIER_assert" => {
        val arg = d.actualParams(LocalVar("R0_in", BitVecType(64)))
        ChangeTo(List(Assert(UnaryExpr(BoolNOT, BinaryExpr(EQ, arg, BitVecLiteral(0, 64))), Some("__VERIFIER_assert"))))
      }
      case d: DirectCall if d.target.procName == "__VERIFIER_assume" => {
        val arg = d.actualParams(LocalVar("R0_in", BitVecType(64)))
        ChangeTo(List(Assume(UnaryExpr(BoolNOT, BinaryExpr(EQ, arg, BitVecLiteral(0, 64))), Some("__VERIFIER_assume"))))
      }
      case _ => SkipChildren()
    }
  }

  visit_prog(ReplaceNondet(), p)

}
