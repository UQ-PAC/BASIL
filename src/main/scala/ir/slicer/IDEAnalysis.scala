package ir.slicer

import ir.*
import analysis.*

import analysis.solvers.*

trait SlicerTransferFunctions(slicingCriterion: Map[CFGPosition, StatementSlice] = Map())
    extends BackwardIDETransferFunctions[Variable, TwoElement, TwoElementLattice] {

  val valuelattice = TwoElementLattice()
  val edgelattice = EdgeFunctionLattice(valuelattice)
  import edgelattice.{IdEdge, ConstEdge}

  private def fold(variables: Iterable[Variable]): Map[DL, EdgeFunction[TwoElement]] = {
    variables.map(v => Left(v) -> ConstEdge(TwoElementTop)).toMap
  }

  def edgesCallToEntry(call: Command, entry: Return)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    val predecessor = call match {
      case s: Statement => s.predecessor
      case j: Jump => j.parent.statements.lastElem
    }

    val params: Map[LocalVar, Variable] = predecessor match {
      case Some(command) => {
        command match {
          case c: DirectCall => c.outParams
          case i: IndirectCall => Map()
          case s: Statement => ???
          case j: Jump => ???
        }
      }
      case None => ???
    }

    d match {
      case Left(value) if params.values.toSet.contains(value) => fold(params.filter(_._2 == value).keys)
      case _ => Map(d -> IdEdge())
    }
  }

  def edgesExitToAfterCall(exit: Procedure, aftercall: DirectCall)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    d match {
      case Left(value: LocalVar) if aftercall.actualParams.contains(value) =>
        fold(aftercall.actualParams(value).variables)
      case _ => Map(d -> IdEdge())
    }
  }

  def edgesCallToAfterCall(call: Command, aftercall: DirectCall)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    d match {
      case Left(_) => Map()
      case _ => Map(d -> IdEdge())
    }
  }

  def edgesOther(n: CFGPosition)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    (n match {
      case p: Procedure => Map(d -> IdEdge())
      case b: Block => Map(d -> IdEdge())
      case a: LocalAssign => {
        d match {
          case Left(value) if value == a.lhs => fold(a.rhs.variables)
          case _ => Map(d -> IdEdge())
        }
      }
      case a: MemoryAssign => {
        d match {
          case Left(value) if value == a.lhs => fold(a.rhs.variables)
          case _ => Map(d -> IdEdge())
        }
      }
      case a: MemoryLoad => {
        d match {
          case Left(value) if value == a.lhs => fold(a.index.variables)
          case _ => Map(d -> IdEdge())
        }
      }
      case m: MemoryStore => {
        d match {
          case Left(value) if m.index.variables.contains(value) => fold(m.value.variables)
          case _ => Map(d -> IdEdge())
        }
      }
      case a: Assume => {
        d match {
          case Left(_) => Map(d -> IdEdge())
          case Right(_) => Map(d -> IdEdge()) ++ fold(a.body.variables)
        }
      }
      case a: Assert => {
        d match {
          case Left(_) => Map(d -> IdEdge())
          case Right(_) => Map(d -> IdEdge()) ++ fold(a.body.variables)
        }
      }
      case c: DirectCall => {
        d match {
          case Left(value) if c.outParams.values.toSet.contains(value) => Map()
          case _ => Map(d -> IdEdge())
        }
      }
      case i: IndirectCall => Map(d -> IdEdge())
      case n: NOP => Map(d -> IdEdge())
      case g: GoTo => Map(d -> IdEdge())
      case r: Return => {
        d match {
          case Left(value: LocalVar) => {
            r.outParams.get(value) match {
              case Some(returnedValue) => fold(returnedValue.variables)
              case None => Map(d -> IdEdge())
            }
          }
          case _ => Map(d -> IdEdge())
        }
      }
      case u: Unreachable => Map(d -> IdEdge())
    }) ++ fold(slicingCriterion.getOrElse(n, Set()))
  }
}

object SlicerTransferFunctions extends SlicerTransferFunctions()

class SlicerAnalysis(program: Program, slicingCriterion: Map[CFGPosition, StatementSlice])
    extends BackwardIDESolver[Variable, TwoElement, TwoElementLattice](program)
    with BackwardIDEAnalysis[Variable, TwoElement, TwoElementLattice]
    with SlicerTransferFunctions(slicingCriterion)
