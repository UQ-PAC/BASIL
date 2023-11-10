package ir

enum PositionType {
  case Before, After, On
}

// intra-procedural
type ILPosition = Procedure | Block | Command
type CFGPosition = Procedure | Block | Command | ProcedureUnknownJump  | ProcedureReturn

// Interprocedural
//  position = (call string) + Position

case class ProcedureUnknownJump(pos: CFGPosition)
case class ProcedureReturn(procedure: Procedure, pos: CFGPosition)

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
          case None => Set(ProcedureUnknownJump(c))
        case i: IndirectCall => i match 
          case IndirectCall(v, parent, ret, label) =>
            if (v.name == "R30") {
              Set(ProcedureReturn(parent.parent, pos))
            } else {
              ret match
                case Some(block) => Set(block)
                case None => Set(ProcedureUnknownJump(pos))
            }
      }
      case b: Block => Set[CFGPosition](b.statements.head())
      case proc: Procedure => Set(proc.blocks.head())
      case _: ProcedureUnknownJump => Set()
      case _: ProcedureReturn => Set()
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
      case proc: Procedure => Set()
      case r: ProcedureUnknownJump => Set(r.pos)
      case r: ProcedureReturn => Set(r.pos)
    }
  }


  //def succ() : Seq(IRCursor) = {
  //  position match {
  //    case p: Procedure => Seq(IRCursor(p.blocks.head))
  //    case b: Block => Seq(IRCursor(b.statements.head))
  //    case s: Statement => Seq(s.parent.statements.ne())

  //  }
}


