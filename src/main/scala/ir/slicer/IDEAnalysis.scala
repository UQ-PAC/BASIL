package ir.slicer

import ir.*
import analysis.*

import analysis.solvers.*

trait SlicerTransferFunctions(slicingCriterion: Map[CFGPosition, StatementSlice])
    extends BackwardIDETransferFunctions[Variable, TwoElement, TwoElementLattice] {

  val valuelattice = TwoElementLattice()
  val edgelattice = EdgeFunctionLattice(valuelattice)
  import edgelattice.{IdEdge, ConstEdge}

  private def fold(variables: Iterable[Variable]): Map[DL, EdgeFunction[TwoElement]] = {
    variables.map(v => Left(v) -> ConstEdge(TwoElementTop)).toMap
  }

  def edgesCallToEntry(call: Command, entry: Return)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    d match {
      case Left(value) => {
        val params = IRWalk.prevCommandInBlock(call) match {
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

        if (params.values.toSet.contains(value))
        then fold(params.filter(_._2 == value).keys)
        else
          value match {
            case g: Global => Map(d -> IdEdge())
            case _ => Map()
          }
      }
      case Right(_) => Map(d -> IdEdge())
    }
  }

  def edgesExitToAfterCall(exit: Procedure, aftercall: DirectCall)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    d match {
      case Left(value: LocalVar) => fold(aftercall.actualParams(value).variables)
      case _ => Map(d -> IdEdge())
    }
  }

  def edgesCallToAfterCall(call: Command, aftercall: DirectCall)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    d match {
      case Left(value: LocalVar) if aftercall.outParams.values.toSet.contains(value) => Map()
      case Left(_: LocalVar) => Map(d -> IdEdge())
      case Left(_) => Map()
      case Right(_) => Map(d -> IdEdge())
    }
  }

  def edgesOther(n: CFGPosition)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    val transferEdge = intraTransferFunctions(n)
    d match {
      case Left(_) => transferEdge(d)
      case Right(_) => transferEdge(d) ++ slicingCriterion.getOrElse(n, Set()).flatMap(v => transferEdge(Left(v))).toMap
    }
  }

  def intraTransferFunctions(n: CFGPosition)(d: DL): Map[DL, EdgeFunction[TwoElement]] = {
    n match {
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
          case Left(_) => Map(d -> IdEdge()) ++ fold(a.body.variables)
          case Right(_) => Map(d -> IdEdge())
        }
      }
      case a: Assert => {
        d match {
          case Left(_) => Map(d -> IdEdge()) ++ fold(a.body.variables)
          case Right(_) => Map(d -> IdEdge())
        }
      }
      case c: DirectCall => Map(d -> IdEdge())
      case i: IndirectCall => {
        d match {
          case Left(value: Global) => Map(d -> IdEdge(), Left(i.target) -> ConstEdge(TwoElementTop))
          case _ => Map(d -> IdEdge())
        }
      }
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
    }
  }

  def restructure(result: Map[DL, EdgeFunction[TwoElement]]): Map[Variable, EdgeFunction[TwoElement]] = {
    result.foldLeft(Map[Variable, EdgeFunction[TwoElement]]()) {
      case (acc, (d, e)) => {
        d match {
          case Left(value) => acc + (value -> e)
          case Right(_) => acc
        }
      }
    }
  }
}

class SlicerTransfers(slicingCriterion: Map[CFGPosition, StatementSlice])
    extends SlicerTransferFunctions(slicingCriterion)

class SlicerAnalysis(program: Program, startingNode: CFGPosition, slicingCriterion: Map[CFGPosition, StatementSlice])
    extends BackwardIDESolver[Variable, TwoElement, TwoElementLattice](program)
    with BackwardIDEAnalysis[Variable, TwoElement, TwoElementLattice]
    with SlicerTransferFunctions(slicingCriterion) {
  override def start: CFGPosition = startingNode
}
