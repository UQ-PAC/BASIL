package gtirb

import Parsers.*
import Parsers.ASLpParser.*
import org.antlr.v4.runtime.{BailErrorStrategy, CharStreams, CommonTokenStream}
import translating.InsnSemantics
import upickle.default.*

private def parse_asl_stmt(line: String): Option[StmtContext] = {
  val lexer = ASLpLexer(CharStreams.fromString(line))
  val tokens = CommonTokenStream(lexer)
  val parser = ASLpParser(tokens)
  parser.setErrorHandler(BailErrorStrategy())
  parser.setBuildParseTree(true)

  Some(util.catchAntlrParseErrors(parser.stmteof().stmt))
}

implicit val insnSemanticsJsonReader: Reader[InsnSemantics] =
  reader[ujson.Value].map[InsnSemantics] {
    case ujson.Arr(x) =>
      val sems = x.map(_.str).map(parse_asl_stmt).toList

      util.functional.sequence(sems) match {
        case None => InsnSemantics.Error("?", "parseError")
        case Some(xs) => InsnSemantics.Result(xs.toList)
      }
    case ujson.Obj(y) =>
      val m = y.get("decode_error") match {
        case Some(ujson.Obj(x)) => x
        case _ => throw Error(s"Bad sem format $y")
      }
      InsnSemantics.Error(m("opcode").str, m("error").str)
    case x =>
      throw Error(s"Bad sem format $x")
  }
