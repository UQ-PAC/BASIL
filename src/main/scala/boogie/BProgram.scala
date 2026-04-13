package boogie

import ir.Sigil
import util.{LogLevel, Logger, PerformanceTimer}

import java.io.{BufferedWriter, FileWriter, StringWriter, Writer}
import java.nio.file.{Files, Paths}
import scala.collection.immutable.Seq
import scala.sys.process.*

case class BProgram(declarations: List[BDeclaration], filename: String) {
  override def toString: String = declarations.flatMap(x => x.toBoogie).mkString(System.lineSeparator())

  def writeToString(w: Writer): Unit = {
    declarations.foreach(x => x.writeToString(w))
  }

  /**
   * Invokes Boogie to verify the current BProgram.
   */
  def verifyBoogie(fname: String = "") = {
    val temp =
      if !fname.isEmpty then Paths.get(fname)
      else Files.createTempFile("basil-boogie-temp", ".bpl")

    val wr = BufferedWriter(FileWriter(temp.toFile))
    try {
      writeToString(wr)
    } finally {
      wr.close()
    }

    val timer = PerformanceTimer("Verify", LogLevel.INFO)
    val cmd = Seq("boogie", "/useArrayAxioms", temp.toString)
    Logger.info(s"Running: ${cmd.mkString(" ")}")

    val output = cmd.!!
    val result = util.boogie_interaction.parseOutput(output)

    Logger.info(result.toString)
    timer.checkPoint("Finish")

    result
  }
}

trait BDeclaration extends HasAttributes {
  override def attributes: List[BAttribute] = List()
  def toBoogie: List[String] = List(toString)

  def functionOps: Set[FunctionOp] = Set()

  final def writeToString(w: Writer): Unit = {
    for (elem <- toBoogie) {
      w.append(elem)
      w.append(System.lineSeparator())
    }
  }
}

case class BTypeDecl(t: CustomBType) extends BDeclaration {
  override def toString() = s"type $t;"
}

case class BProcedure(
  name: String,
  in: List[BVar] = Nil,
  out: List[BVar] = Nil,
  ensures: List[BExpr] = Nil,
  requires: List[BExpr] = Nil,
  ensuresDirect: List[String] = Nil,
  requiresDirect: List[String] = Nil,
  freeEnsures: List[BExpr] = Nil,
  freeRequires: List[BExpr] = Nil,
  modifies: Set[BVar] = Set(),
  body: List[BCmdOrBlock] = Nil,
  override val attributes: List[BAttribute] = List()
) extends BDeclaration
    with Ordered[BProcedure] {
  override def compare(that: BProcedure): Int = name.compare(that.name)
  override def toBoogie: List[String] = {
    val header = s"procedure $attrString${Sigil.Boogie.proc}$name(${in.map(_.withType).mkString(", ")})"
    val implHeader = s"implementation $attrString${Sigil.Boogie.proc}$name(${in.map(_.withType).mkString(", ")})"
    val returns = if (out.nonEmpty) {
      s" returns (${out.map(_.withType).mkString(", ")})"
    } else {
      ""
    }
    val modifiesStr = if (modifies.nonEmpty) {
      List(s"  modifies ${modifies.toSeq.sorted.mkString(", ")};")
    } else {
      List()
    }
    val requiresStrs = requires.map(r =>
      s"  requires ${r};${r.label.map(l => s" // ${l.toString}").getOrElse("")}"
    ) ++ requiresDirect.map(r => s"  requires $r;")
    val ensuresStrs = ensures.map(e =>
      s"  ensures ${e};${e.label.map(l => s" // ${l.toString}").getOrElse("")}"
    ) ++ ensuresDirect.map(e => s"  ensures $e;")
    val freeRequiresStrs = freeRequires.map(r => s"  free requires $r;")
    val freeEnsuresStrs = freeEnsures.map(e => s"  free ensures $e;")
    val locals: Set[BVar] = (body.flatMap(l => l.locals).toSet) -- (in.toSet ++ out.toSet)
    val localDefs = locals.toList.sorted.map(l => "  " + BVarDecl(l).toString)
    val bodyStr = if (body.nonEmpty) {
      List("{") ++ localDefs ++ body.flatMap(x => x.toBoogie).map(s => "  " + s) ++ List("}")
    } else {
      List()
    }

    val procDecl = s"$header$returns;"
    val procList = List(procDecl) ++ modifiesStr ++ requiresStrs ++ freeRequiresStrs ++ ensuresStrs ++ freeEnsuresStrs
    val implDecl = s"$implHeader$returns"
    val implList = if (body.nonEmpty) {
      List("", implDecl) ++ bodyStr
    } else {
      List()
    }

    procList ++ implList ++ List("")
  }
  override def toString: String = toBoogie.mkString("\n")
  override def functionOps: Set[FunctionOp] = {
    val bodyOps = body.flatMap(_.functionOps)
    val ensuresOps = ensures.flatMap(_.functionOps) ++ freeEnsures.flatMap(_.functionOps)
    val requiresOps = requires.flatMap(_.functionOps) ++ freeRequires.flatMap(_.functionOps)
    (bodyOps ++ ensuresOps ++ requiresOps).toSet
  }

  def globals: Set[BVar] = {
    val bodyGlobals = body.flatMap(_.globals)
    val ensuresGlobals = ensures.flatMap(_.globals) ++ freeEnsures.flatMap(_.globals)
    val requiresGlobals = requires.flatMap(_.globals) ++ freeRequires.flatMap(_.globals)
    (bodyGlobals ++ ensuresGlobals ++ requiresGlobals).toSet ++ modifies
  }
}

case class BAxiom(body: BExpr, override val attributes: List[BAttribute] = List()) extends BDeclaration {
  override def toString: String = s"axiom $attrString$body;"
}

case class BFunction(
  name: String,
  in: List[BVar],
  out: BVar,
  body: Option[BExpr],
  override val attributes: List[BAttribute] = List()
) extends BDeclaration
    with Ordered[BFunction] {
  override def compare(that: BFunction): Int = name.compare(that.name)
  override def toBoogie: List[String] = {
    val s = StringWriter()

    val inString = in.map(_.withType).mkString(", ")
    val declString = s"function $attrString$name($inString) returns (${out.withType})"
    s.append(declString)

    val decl = body match {
      case Some(b) =>
        s.append(" {" + System.lineSeparator() + "  ")
        b.serialiseBoogie(s)
        s.append(System.lineSeparator())
        s.append("}" + System.lineSeparator())
      case None => s.append(";")
    }
    List(s.toString)
  }
  override def toString: String = toBoogie.mkString("\n")
  override def functionOps: Set[FunctionOp] = body match {
    case Some(b) => b.functionOps
    case None => Set()
  }
}

case class BVarDecl(variable: BVar, override val attributes: List[BAttribute] = List())
    extends BDeclaration
    with Ordered[BVarDecl] {
  def compare(that: BVarDecl): Int = variable.compare(that.variable)
  override def toString: String = if (variable.scope == Scope.Const) {
    s"const $attrString$variable: ${variable.getType};"
  } else {
    s"var $attrString$variable: ${variable.getType};"
  }
}

case class BConstAxiomPair(const: BVarDecl, axiom: BAxiom) extends BDeclaration with Ordered[BConstAxiomPair] {
  override def compare(that: BConstAxiomPair): Int = const.compare(that.const)
  override def toString: String = const.toString + "\n" + axiom.toString
}

case class BDataTypeConstructor(name: String, fields: List[BVar]) {
  val fieldsString = fields.map(_.withType).mkString(", ")
  val declString = s"$name($fieldsString)"

  override def toString: String = declString
}

case class BDataTypeDecl(name: String, constructors: List[BDataTypeConstructor]) extends BDeclaration {
  val constructorsString = constructors.mkString(", ");
  val declString = s"datatype $name {$constructorsString}";
  override def toString: String = declString
  override def toBoogie: List[String] = List(toString)
}
