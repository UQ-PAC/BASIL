package ir.dsl

import ir.*
import util.{Twine, indentNested}

/**
 * The end of this file contains generated code to implement ToScalaLines for the various type
 * hierarchies defined by BASIL.
 */

/**
 * XXX: ATTENTION: auto-generated code!
 * ====================================
 *
 * If you are here to manually fix a compilation error, please make sure that
 * you understand the context and carefully apply a specific fix. Make sure
 * that the code maintains the general contract of the ToScalaLines trait.
 *
 * That is, the returned string must be valid Scala code to construct the given
 * object. The `ensure_constructible` functions are defined to match the produced
 * string. Successful compilation of the ensure_constructible functions ensures
 * that the produced strings are valid Scala code.
 *
 * When making manual changes, be sure to change the string literal and the
 * ensure_constrictible functions in the same way.
 */

/**
 * Running the auto-generator
 * --------------------------
 *
 * For large changes, it may be more convenient to re-run the auto-generator
 * instead of manually changing the code.
 *
 * 1. Look below the scissors line (containing "- >8 -") to find the "command:" line.
 *    Take note of which JSON files are mentioned.
 * 2. For each JSON file, find its corresponding Scala file (e.g. statements.json comes
 *    from Statement.scala).
 * 3. Go to https://astexplorer.net/#/gist/eb0f2062180067b412017010df04eace/latest and
 *    make sure that the scalameta setting is set to "Scala 3".
 * 4. Paste the contents of the Scala file into the left side.
 * 5. Copy the JSON from the right-hand side into a new JSON file.
 * 6. Repeat this for the rest of the required JSON files.
 *
 * 7. Run the Python command listed below the scissors line.
 * 8. Make sure that the code compiles and passes tests:
 *
 *     ./mill test.testOnly 'ir.ToScalaLinesTest'
 *
 * 9. Upload the changes.
 *
 */

/**
 * Manually-defined ToScalaLines instances
 * ----------------------------------
 *
 * Externals: DirectCall, IndirectCall, GoTo, Return
 *
 * The "Externals" line above tells the generator the following types as having
 * manually-defined ToScalaLines instances. These types interact with control-flow,
 * and the naive auto-generated code would be large and possibly cyclic.
 */

// NOTE: It is important that these handwritten given instances live in the same
// file as the generated code. These instances must be locatable by summon[],
// otherwise the generated code will self-recurse, leading to non-termination.

/**
 * Automatically-derived instances
 * -------------------------------
 *
 * For some types (namely the sealed traits, enums, and case classes), the ToScala
 * instances can be automatically derived. See ToScalaDeriving.
 *
 */

given ToScala[Expr] = ToScala.derived
given ToScala[UnOp] = ToScala.derived
given ToScala[BinOp] = ToScala.derived
given ToScala[Endian] = ToScala.derived
given ToScala[Global] = ToScala.derived
given ToScala[IRType] = ToScala.derived


// NOTE: Unfortunately, for the Command trait, this is not straightforward because the classes are not case classes.

/**
 * A hierarchy mirroring BASIL IR statements but it's case classes so we can use automatic deriving.
 */
private object CaseIR {

  import collection.immutable.{Map,SortedMap}
  import collection.mutable

  sealed trait Command

  sealed trait Statement extends Command
  sealed trait Assign extends Statement
  sealed trait SingleAssign extends Assign
  sealed trait Jump extends Command
  sealed trait Call extends Statement

  case class LocalAssign(lhs: Variable, rhs: Expr, label: Option[String] = None) extends SingleAssign
  case class MemoryStore(mem: Memory, index: Expr, value: Expr, endian: Endian, size: Int, label: Option[String] = None) extends Statement
  case class MemoryLoad(lhs: Variable, mem: Memory, index: Expr, endian: Endian, size: Int, label: Option[String] = None) extends SingleAssign
  case class NOP(label: Option[String] = None) extends Statement
  case class Assert(body: Expr, comment: Option[String] = None, label: Option[String] = None) extends Statement
  case class Assume(body: Expr, comment: Option[String] = None, label: Option[String] = None, checkSecurity: Boolean = false) extends Statement
  case class Unreachable(label: Option[String] = None) extends Jump
  case class Return(label: Option[String] = None, outParams : Map[LocalVar, Expr] = SortedMap()) extends Jump
  case class GoTo (targets: Set[Block], label: Option[String]) extends Jump
  case class DirectCall(target: Procedure, label: Option[String] = None, outParams: Map[LocalVar, Variable] = SortedMap(), actualParams: Map[LocalVar, Expr] = SortedMap()) extends Call with Assign
  case class IndirectCall(target: Variable, label: Option[String] = None) extends Call


  def fromBasilIR(x: ir.Command): Command = x match {
    case ir.LocalAssign(a,b,c) => LocalAssign(a,b,c)
    case ir.MemoryStore(a,b,c,d,e,f) => MemoryStore(a,b,c,d,e,f)
    case ir.MemoryLoad(a,b,c,d,e,f) => MemoryLoad(a,b,c,d,e,f)
    case ir.NOP(a) => NOP(a)
    case ir.Assert(a,b,c) => Assert(a,b,c)
    case ir.Assume(a,b,c,d) => Assume(a,b,c,d)
    case ir.Unreachable(a) => Unreachable(a)
    case ir.Return(a,b) => Return(a,b)
    case ir.GoTo(a,b) => GoTo(a,b)
    // XXX: order changed in DirectCall params
    case ir.DirectCall(a,b,c,d) => DirectCall(a,d,b,c)
    case ir.IndirectCall(a,b) => IndirectCall(a,b)
  }


  type Excluded = Return | DirectCall | IndirectCall | GoTo

  private lazy val toScalaOfExcluded = ToScala.Make[Excluded] {
    case x: Return => {
      if (x.outParams.isEmpty) {
        LazyList("ret")
      } else {
        def outParamToScala(x: (LocalVar, Expr)) = (x(0).name, x(1)).toScalaLines
        indentNested("ret(", x.outParams.map(outParamToScala), ")")
      }
    }
    case x: DirectCall => LazyList(s"directCall(${x.target.procName.toScala})")
    case x: IndirectCall => LazyList(s"indirectCall(${x.target.toScala})")
    case x: GoTo => LazyList(s"goto(${x.targets.map(x => x.label.toScala).mkString(", ")})")
  }

  given ToScala[Command] = ToScala.deriveWithExclusions[Command, Excluded](toScalaOfExcluded)

}

given ToScala[ir.Command] = ToScala.Make(x => CaseIR.fromBasilIR(x).toScalaLines)

