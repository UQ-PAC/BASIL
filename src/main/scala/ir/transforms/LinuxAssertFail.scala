package ir.transforms

import ir.*
import ir.eval.*
import util.functional.*
import util.IRContext
import ir.cilvisitor.*

def liftLinuxAssertFail(ctx: IRContext) = {
  /**
   * https://refspecs.linuxfoundation.org/LSB_1.3.0/gLSB/gLSB/baselib---assert-fail-1.html
   */

  val begin = InterpFuns.initProgState(NormalInterpreter)(ctx, InterpreterState())
  def getString(x: BitVecLiteral) : Option[String] = {
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

  case class AssertFail(info: Option[String], filename: Option[String], lineNo: Option[Int], function: Option[String]) 

  def getBV(e: Expr) = e match {
    case b: BitVecLiteral => Some(b)
    case _ => None
  }

  val asserts = ctx.program.collect {
    case d: DirectCall if d.target.procName == "__assert_fail" 
      => {
        d -> AssertFail(
          getBV(d.actualParams(assertParam)).flatMap(getString), 
          getBV(d.actualParams(fileParam)).flatMap(getString), 
          getBV(d.actualParams(lineNo)).map(_.value.toInt),
          getBV(d.actualParams(funNameParam)).flatMap(getString), 
        )
      }
  }.toMap

  class Replacer extends CILVisitor {

    override def vstmt(s : Statement) = {
      s match {
        case d : DirectCall if asserts.contains(d) => {
          val af = asserts(d)
          val msg = af.info.getOrElse("")
          val line = af.filename.map(fn => s" $fn:${af.lineNo.map(_.toString).getOrElse("?")}").getOrElse("")
          val fun = af.function.map(f => s" @ $f").getOrElse("")
          ChangeTo(List(Assert(FalseLiteral, Some(s"$msg$fun$line"))))
        }
        case _ => SkipChildren()
      }
    }
  }

  visit_prog(Replacer(), ctx.program)
}

