package util.z3
import scala.sys.process.*
import java.io.ByteArrayInputStream

enum SatResult {
  case SAT(errors: List[String])
  case UNSAT
  case Unknown(s: String, errors: List[String])
}

def checkSATSMT2(smt: String, softTimeoutMillis: Option[Int] = None): SatResult = {
  val cmd =
    Seq("z3", "-smt2", "-in") ++ (if softTimeoutMillis.isDefined then Seq(s"-t:${softTimeoutMillis.get}") else Seq())
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
