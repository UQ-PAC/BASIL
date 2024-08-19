package ir.transforms

import util.Logger
import ir.cilvisitor._
import ir._


class ReplaceReturns extends CILVisitor {
  /**
   * Assumes IR with 1 call per block which appears as the last statement.
   */
  override def vstmt(j: Statement): VisitAction[List[Statement]] = {
    j match {
      case IndirectCall(Register("R30", _), _) => {
        assert(j.parent.statements.lastOption.contains(j))
        if (j.parent.jump.isInstanceOf[Halt | Return]) {
          j.parent.replaceJump(Return())
          ChangeTo(List())
        } else {
          SkipChildren()
        }
      }
      case _ => SkipChildren()
    }
  }

  override def vjump(j: Jump) = SkipChildren()
}


def addReturnBlocks(p: Program) = {
  p.procedures.foreach(p => {
    val containsReturn = p.blocks.map(_.jump).find(_.isInstanceOf[Return]).isDefined
    if (containsReturn) {
      p.returnBlock = p.addBlocks(Block(label=p.name + "_return",jump=Return()))
    }
  })
}


class ConvertSingleReturn extends CILVisitor {
  /**
   * Assumes procedures have defined return blocks if they contain a return statement.
   */
  override def vjump(j: Jump) = j match {
    case r: Return if !(j.parent.parent.returnBlock.contains(j.parent)) => ChangeTo(GoTo(Seq(j.parent.parent.returnBlock.get)))
    case _ => SkipChildren()
  }

  override def vstmt(s: Statement) = SkipChildren()
}

