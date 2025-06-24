package gtirb

import util.Logger
import translating.InsnSemantics

import upickle.default._

import Parsers.*
import Parsers.ASLpParser.*
import org.antlr.v4.runtime.BailErrorStrategy
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}

private def parse_asl_stmt(line: String): Option[StmtContext] = {
  val lexer = ASLpLexer(CharStreams.fromString(line))
  val tokens = CommonTokenStream(lexer)
  val parser = ASLpParser(tokens)
  parser.setErrorHandler(BailErrorStrategy())
  parser.setBuildParseTree(true)

  try {
    Some(parser.stmteof().stmt)
  } catch {
    case e: org.antlr.v4.runtime.misc.ParseCancellationException =>
      val extra = e.getCause match {
        case mismatch: org.antlr.v4.runtime.InputMismatchException =>
          val token = mismatch.getOffendingToken
          s"""
            exn: $mismatch
            offending token: $token

          ${line.replace('\n', ' ')}
          ${" " * token.getStartIndex}^ here!
          """.stripIndent
        case o => o.toString
      }
      Logger.error(s"""Semantics parse error:\n  line: $line\n$extra""")
      Logger.error(e.getStackTrace.mkString("\n"))
      None
  }
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
