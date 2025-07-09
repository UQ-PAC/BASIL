package util.z3
import java.io.ByteArrayInputStream
import scala.sys.process.*

enum SatResult {
  case SAT(errors: List[String])
  case UNSAT
  case Unknown(s: String, errors: List[String])
}

def checkSATSMT2(smt: String, softTimeoutMillis: Option[Int] = None, timeoutSec: Option[Int] = None): SatResult = {
  val t = timeoutSec match {
    case Some(n) => Seq(s"-T:$n")
    case _ => Seq()
  }
  val softT = softTimeoutMillis match {
    case Some(n) => Seq(s"-t:$n")
    case None => Seq()
  }
  val cmd =
    Seq("z3", "-smt2", "-in") ++ t ++ softT
  val output = (cmd #< ByteArrayInputStream(smt.getBytes("UTF-8"))).!!
  val errors = output.split("\n").filter(_.trim.startsWith("(error")).toList
  val outputStripped = output.stripLineEnd
  if (outputStripped == "sat") {
    SatResult.SAT(errors)
  } else if (outputStripped == "unsat") {
    SatResult.UNSAT
  } else {
    SatResult.Unknown(output, errors)
  }
}
