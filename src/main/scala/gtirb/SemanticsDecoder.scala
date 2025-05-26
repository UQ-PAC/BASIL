package gtirb

import upickle.default._
import translating.InsnSemantics

import util.Logger

import Parsers.*
import Parsers.ASLpParser.*
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.antlr.v4.runtime.BailErrorStrategy
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream, Token}

private def parse_asl_stmt(line: String): Option[StmtContext] = {
  val lexer = ASLpLexer(CharStreams.fromString(line))
  val tokens = CommonTokenStream(lexer)
  val parser = ASLpParser(tokens)
  parser.setErrorHandler(BailErrorStrategy())
  parser.setBuildParseTree(true)

  try {
    Some(parser.stmt())
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

      util.functional.sequence(Option)(sems) match {
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

// implicit object InsnSemanticsFormat extends JsonFormat[InsnSemantics] {
//   def write(m: InsnSemantics): JsValue = ???
//   def read(json: JsValue): InsnSemantics = json match {
//     case JsObject(fields) =>
//       val m: Map[String, JsValue] = fields.get("decode_error") match {
//         case Some(JsObject(m)) => m
//         case _ => deserializationError(s"Bad sem format $json")
//       }
//       InsnSemantics.Error(m("opcode").convertTo[String], m("error").convertTo[String])
//     case array @ JsArray(_) =>
//       val xs = array.convertTo[Array[String]].map(parse_asl_stmt)
//       if (xs.exists(_.isEmpty)) {
//         InsnSemantics.Error("?", "parseError")
//       } else {
//         InsnSemantics.Result(xs.map(_.get))
//       }
//     case s => deserializationError(s"Bad sem format $s")
//   }
// }
