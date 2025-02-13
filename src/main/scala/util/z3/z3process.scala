package util.z3
import scala.sys.process.*
import java.io.ByteArrayInputStream
import util.Logger
import scala.collection.mutable.Stack
import ir.Expr

enum SatResult {
  case SAT
  case UNSAT
  case Unknown(s: String)
}

case class SatTest(expr: Expr, comment: Option[String] = None)

def checkSatExprBatch(smt: List[SatTest], softTimeoutMillis: Option[Int] = None) : List[(Option[String], SatResult)] = {

  val queries = smt.map(test => translating.BasilIRToSMT2.exprUnsat(test.expr, test.comment, false))
  val query = queries.mkString("\n")
  val cmd =
    Seq("z3", "-smt2", "-in") ++ (if (softTimeoutMillis.isDefined) then Seq(s"-t:${softTimeoutMillis.get}") else Seq())

  val output = (cmd #< ByteArrayInputStream(query.getBytes("UTF-8"))).!!

  val lines = output.split("\n")
  val stack = Stack[String]()
  stack.addAll(lines)

  val res  = smt.map(t => {
    val comment = if t.comment.isDefined then Some(stack.pop()) else None
    val output = stack.pop()
    val returned = if (output == "sat\n") {
      SatResult.SAT
    } else if (output == "unsat\n") {
      SatResult.UNSAT
    } else {
      SatResult.Unknown(output)
    }
    (comment, returned)
  })
  res

}

def checkSATSMT2(smt: String, softTimeoutMillis: Option[Int] = None): SatResult = {
  val cmd =
    Seq("z3", "-smt2", "-in") ++ (if (softTimeoutMillis.isDefined) then Seq(s"-t:${softTimeoutMillis.get}") else Seq())
  val output = (cmd #< ByteArrayInputStream(smt.getBytes("UTF-8"))).!!
  if (output == "sat\n") {
    SatResult.SAT
  } else if (output == "unsat\n") {
    SatResult.UNSAT
  } else {
    SatResult.Unknown(output)
  }
}
