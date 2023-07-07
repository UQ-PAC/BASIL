package cfg_visualiser

import java.io.{File, PrintWriter}
import analysis._


/**
  * Basic outputting functionality.
  */
object Output {

  /**
    * Generate an output to a file.
    * @param file the output file
    * @param kind output kind (determines the file name suffix)
    * @param outFolder the output directory
    */
  def output(kind: OutputKind, content: String, fileName: String): Unit = {
    val extension = kind match {
      case OtherOutput(OutputKindE.`cfg`) => "_cfg.dot"
      case OtherOutput(OutputKindE.`icfg`) => "_icfg.dot"
//      case DataFlowOutput(k) =>
//        s"_$k.dot"
      case _ => ???
    }
    val outFile = new File(s"${fileName}.dot")
    val pw = new PrintWriter(outFile, "UTF-8")
    pw.write(content)
    pw.close()
  }

  /**
    * Escapes special characters in the given string.
    * Special characters are all Unicode chars except 0x20-0x7e but including \, ", {, and }.
    */
  def escape(s: String): String = {
    if (s == null)
      return null
    val b = new StringBuilder()
    for (i <- 0 until s.length) {
      val c = s.charAt(i)
      c match {
        case '"' =>
          b.append("\\\"")
        case '\\' =>
          b.append("\\\\")
        case '\b' =>
          b.append("\\b")
        case '\t' =>
          b.append("\\t")
        case '\n' =>
          b.append("\\n")
        case '\r' =>
          b.append("\\r")
        case '\f' =>
          b.append("\\f")
        case '<' =>
          b.append("\\<")
        case '>' =>
          b.append("\\>")
        case '{' =>
          b.append("\\{")
        case '}' =>
          b.append("\\}")
        case _ =>
          if (c >= 0x20 && c <= 0x7e)
            b.append(c)
          else {
            b.append("\\%04X".format(c.toInt))
          }
      }
    }
    b.toString()
  }

  /**
    * Helper function for producing string output for a control-flow graph node after an analysis.
    * @param res map from control-flow graph nodes to strings, as produced by the analysis
    */
  def labeler(res: Map[CfgNode, _], stateAfterNode: Boolean)(n: CfgNode): String = {
    val r = res.getOrElse(n, "-")
    val desc = n match {
      case entry: CfgFunctionEntryNode => s"Function ${entry.data.name} entry"
      case exit: CfgFunctionExitNode => s"Function ${exit.data.name} exit"
      case _ => n.toString
    }
    if (stateAfterNode) s"$desc\n$r"
    else s"$r\n$desc"
  }

  /**
    * Generate an unique ID string for the given AST node.
    */
  def dotIder(n: CfgNode, uniqueId: Int): String =
    n match {
      case real: CfgCommandNode => s"real${real.data}_${uniqueId}"
      case entry: CfgFunctionEntryNode => s"entry${entry.data}_${uniqueId}"
      case exit: CfgFunctionExitNode => s"exit${exit.data}_${uniqueId}"
      case _ => ???
    }
}

/**
  * Different kinds of output (determine output file names).
  */
object OutputKindE extends Enumeration {
  val cfg, icfg = Value
}

sealed trait OutputKind

///**
//  * Output kind for a dataflow analysis (named according to the analysis).
//  */
//case class DataFlowOutput(kind: FlowSensitiveAnalysis.Analysis.Value) extends OutputKind {
//  override def toString: String = kind.toString
//}

/**
  * Other output kinds (for other processing phases than the actual analysis).
  */
case class OtherOutput(kind: OutputKindE.Value) extends OutputKind {
  override def toString: String = kind.toString
}
