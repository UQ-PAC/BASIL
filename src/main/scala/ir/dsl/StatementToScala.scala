package ir.dsl

import ir.*
import util.twine.Twine

/**
 * ToScala for Statement and Expr
 * ==============================
 * This file contains code to implement ToScala for the various type
 * hierarchies defined by BASIL. Most of these are implemented using the automatic
 * deriving mechanism, but Basil IR Commands have to be translated to a case class
 * version before using the automatic deriving.
 */

/**
 * Automatically-derived instances
 * -------------------------------
 *
 * For some types (namely the sealed traits, enums, and case classes), the ToScala
 * instances can be automatically derived. See ToScalaDeriving.
 *
 */

given ToScala[QuantifierSort] = ToScala.derived
given ToScala[Expr] = ToScala.derived
given ToScala[UnOp] = ToScala.derived
given ToScala[BinOp] = ToScala.derived
given ToScala[Endian] = ToScala.derived
given ToScala[IRType] = ToScala.derived

// NOTE: Unfortunately, for the Command trait, this is not straightforward because the classes are not case classes.

/**
 * A hierarchy mirroring BASIL IR statements but it's case classes so we can use automatic deriving.
 */
private object CaseIR {

  import collection.immutable.{Map, SortedMap}

  // format: off

  // NOTE: format off because this is easier to scan and manipulate (e.g. with vim)
  // when we have the structure of one line per case

  sealed trait Command

  sealed trait Statement extends Command
  sealed trait Assign extends Statement
  sealed trait SingleAssign extends Assign
  sealed trait Jump extends Command
  sealed trait Call extends Statement

  case class SimulAssign(lhs: List[(Variable, Expr)], label: Option[String] = None) extends Assign
  case class LocalAssign(lhs: Variable, rhs: Expr, label: Option[String] = None) extends SingleAssign
  case class MemoryAssign(lhs: Variable, rhs: Expr, label: Option[String] = None) extends SingleAssign
  case class MemoryStore(mem: Memory, index: Expr, value: Expr, endian: Endian, size: Int, label: Option[String] = None) extends Statement
  case class MemoryLoad(lhs: Variable, mem: Memory, index: Expr, endian: Endian, size: Int, label: Option[String] = None) extends SingleAssign
  case class NOP(label: Option[String] = None) extends Statement
  case class Assert(body: Expr, comment: Option[String] = None, label: Option[String] = None) extends Statement
  case class Assume(body: Expr, comment: Option[String] = None, label: Option[String] = None, checkSecurity: Boolean = false) extends Statement
  case class Unreachable(label: Option[String] = None) extends Jump
  case class Return(label: Option[String] = None, outParams : Map[LocalVar, Expr] = SortedMap()) extends Jump
  case class GoTo (targets: collection.Set[Block], label: Option[String]) extends Jump
  case class DirectCall(target: Procedure, label: Option[String] = None, outParams: Map[LocalVar, Variable] = SortedMap(), actualParams: Map[LocalVar, Expr] = SortedMap()) extends Call with Assign
  case class IndirectCall(target: Variable, label: Option[String] = None) extends Call

  def fromBasilIR(x: ir.Command): Command = x match {
    case ir.LocalAssign(a,b,c) => LocalAssign(a,b,c)
    case ir.SimulAssign(a,b) => SimulAssign(a.toList,b)
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
    case ir.MemoryAssign(lhs, rhs, l) => MemoryAssign(lhs, rhs, l)
  }

  // format: on

  /**
   * Manually-defined ToScala instances
   * ----------------------------------
   *
   * These types interact with control-flow, and the naive auto-generated
   * code would be large and possibly cyclic.
   */

  type Excluded = Return | DirectCall | IndirectCall | GoTo

  private lazy val toScalaOfExcluded = ToScala.Make[Excluded] {
    case Return(label, outs) => {
      if (outs.isEmpty) {
        Twine("ret")
      } else {
        given ToScala[LocalVar] = ToScala.MakeString(_.name.toScala)

        Twine.indentNested("ret(", outs.map(_.toScalaLines), ")")
      }
    }
    case DirectCall(tgt, label, outs, actuals) =>
      if (outs.isEmpty && actuals.isEmpty) {
        Twine(s"directCall(${tgt.procName.toScala})")
      } else {
        given ToScala[LocalVar] = ToScala.MakeString(_.name.toScala)

        Twine.indentNested(
          s"directCall(",
          List(outs.toSeq.toScalaLines, tgt.name.toScalaLines, actuals.toSeq.toScalaLines),
          ")"
        )
      }
    case x: IndirectCall => Twine(s"indirectCall(${x.target.toScala})")
    case x: GoTo => Twine(s"goto(${x.targets.map(x => x.label.toScala).mkString(", ")})")
  }

  given ToScala[Command] = ToScala.deriveWithExclusions[Command, Excluded](toScalaOfExcluded)

}

/**
 * ToScala[Command] is implemented in terms of the automatiicaly-derived
 * ToScala[CaseIR.Command], with certain exclusions for control-flow-affecting
 * commands.
 */
given ToScala[ir.Command] = ToScala.Make(x => CaseIR.fromBasilIR(x).toScalaLines)
