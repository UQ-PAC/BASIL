package ir
import collection.mutable

/*
 * Defines a position in the IL / CFG; this becomes the lhs of the state map lattice in a static analysis.
 */
type CFGPosition = Procedure | Block | Command | ProcedureUnknownJump  | ProcedureExit


// Interprocedural
//  position = (call string) + Position


/*
    An additional CFG node which implicitly follows the node at `pos`
    A call to an unknown procedure without a return to here
 */
case class ProcedureUnknownJump(fromProcedure: Procedure, pos: CFGPosition)

/*
 *  An additional CFG node which implicitly follows the node at `pos`
 *  The exit from a procedure from pos (the last command/jump in the procedure).
 */
case class ProcedureExit(fromProcedure: Procedure, pos: CFGPosition)

object IntraProcIRCursor {
  type Node = CFGPosition

  def succ(pos: CFGPosition): Set[CFGPosition] = {
    pos match {
      case s: Statement =>
        if (s.parent.statements.hasNext(s)) {
          Set(s.parent.statements.getNext(s))
        } else {
          s.parent.jumps.toSet
        }
      case j: Jump => j match {
        /* TODO jumps are ordered so prior jumps mask later jumps; assuming the union of the conditions is total 
         * This will not be the case once we make all jumps nondeterministic. */
        case g: DetGoTo => Set[CFGPosition](g.target)
        case n: NonDetGoTo => n.targets.toSet
        case c: DirectCall => c.returnTarget match
          case Some(b) => Set(b)
          case None => Set(ProcedureUnknownJump(c.parent.parent, c))
        case i: IndirectCall => i match 
          case IndirectCall(v, parent, ret, label) =>
            if (v.name == "R30") {
              Set(ProcedureExit(parent.parent, pos))
            } else {
              ret match
                case Some(block) => Set(block)
                case None => Set(ProcedureUnknownJump(i.parent.parent, pos))
            }
      }
      case b: Block => if b.statements.isEmpty then Set(b.jumps.head) else Set[CFGPosition](b.statements.head())
      case proc: Procedure => if proc.blocks.isEmpty then Set(ProcedureExit(proc, proc)) else Set(proc.blocks.head())
      case j: ProcedureUnknownJump => Set(ProcedureExit(j.fromProcedure, j))
      case e: ProcedureExit => Set()
    }
  }

  def pred(pos: CFGPosition): Set[CFGPosition] = {
    pos match {
      case s: Statement =>
        if (s.parent.statements.hasPrev(s)) {
          Set(s.parent.statements.getPrev(s))
        } else {
          Set(s.parent) // predecessor blocks
        }
      case j: Jump => if j.parent.statements.isEmpty then Set(j.parent) else Set(j.parent.statements.last)
      case b: Block => b.predecessors.asInstanceOf[Set[CFGPosition]]
      case proc: Procedure => Set() // intraproc
      case r: ProcedureUnknownJump => Set(r.pos)
      case r: ProcedureExit => Set(r.pos)
    }
  }
}


def computeDomain(prog: Program): mutable.Set[CFGPosition] = {
  val domain : mutable.Set[CFGPosition] = mutable.Set.from(prog.procedures)

  var sizeBefore = 0
  var sizeAfter = domain.size
  while (sizeBefore != sizeAfter) {
    for (i <- domain) {
      domain.addAll(IntraProcIRCursor.succ(i))
    }
    sizeBefore = sizeAfter
    sizeAfter = domain.size
  }
  domain
} 

