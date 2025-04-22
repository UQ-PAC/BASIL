package ir.slicer

import ir.*

type SlicingParameter = Variable 

type StatementSlice = Set[SlicingParameter]
object StatementSlice {
  def apply(): StatementSlice = Set.empty[SlicingParameter]
}

class Summary(
  var _entry: Either[() => StatementSlice, StatementSlice] = Right(StatementSlice()),
  var _exit: Either[() => StatementSlice, StatementSlice] = Right(StatementSlice())
) {
  def entry: StatementSlice = {
    _entry match {
      case Left(e) => e.apply()
      case Right(e) => e
    }
  }

  def exit: StatementSlice = {
    _exit match {
      case Left(e) => e.apply()
      case Right(e) => e
    }
  }
}

def build(
  results: Map[CFGPosition, StatementSlice],
  slicingCriterion: Map[CFGPosition, StatementSlice]
): Map[CFGPosition, Summary] = {

  def flatten(ns: Iterable[CFGPosition]): StatementSlice = {
    ns.toList.flatMap(b => results.get(b).toList) match {
      case Nil => Set.empty
      case h :: Nil => h
      case h :: tl => tl.foldLeft(h)((acc, nb) => acc.union(nb))
    }
  }

  def summary(n: CFGPosition): Summary = {
    val result = results.getOrElse(n, StatementSlice())
    val criterion = slicingCriterion.getOrElse(n, StatementSlice())

    n match {
      case p: Procedure => {
        p.returnBlock match {
          case Some(block) => Summary(Left(() => summary(block).entry), Right(result ++ criterion))
          case None => ??? //Summary()
        }
      }
      case b: Block => {
        b.nextBlocks match {
          case Nil => Summary(Left(() => summary(b.jump).entry), Right(result ++ criterion))
          case blocks => Summary(Right(flatten(blocks)), Right(result ++ criterion))
        }
      }
      case c: DirectCall => Summary(Left(() => summary(c.successor).exit), Right(result ++ criterion))
      case g: GoTo => Summary(Right(flatten(g.targets)), Right(result ++ criterion))
      case r: Return => {
        Summary(
          Right(result),
          r.parent.statements.lastElem match {
            case Some(statement) => Left(() => summary(statement).entry ++ criterion)
            case None => Left(() => summary(r.parent).exit ++ criterion)
          }
        )
      }
      // case s: Statement => {
      case s: (LocalAssign | MemoryAssign | MemoryLoad | MemoryStore | Assume | Assert) => {
        Summary(
          Right(result),
          s.predecessor match {
            case Some(pred) => Left(() => summary(pred).entry ++ criterion)
            case None => Left(() => summary(s.parent).exit ++ criterion)
          }
        )
      }
      case i: IndirectCall => Summary(Right(result), Right(result ++ criterion))
      case u: Unreachable => Summary(Right(result), Right(result ++ criterion))
      case n: NOP => Summary(Right(result), Right(result ++ criterion))
    }
  }

  results
    .map((n, result) => n -> summary(n))
    .toMap
    .withDefault(n => summary(n))
}

