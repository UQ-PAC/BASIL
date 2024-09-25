package ir.transforms

import util.Logger
import ir.cilvisitor.*
import ir.*


class ReplaceReturns extends CILVisitor {
  /**
   * Assumes IR with 1 call per block which appears as the last statement.
   */
  override def vstmt(j: Statement): VisitAction[List[Statement]] = {
    j match {
      case IndirectCall(Register("R30", _), _) => {
        assert(j.parent.statements.lastOption.contains(j))
        if (j.parent.jump.isInstanceOf[Unreachable | Return]) {
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


def addReturnBlocks(p: Program, toAll: Boolean = false) = {
  p.procedures.foreach(p => {
    val containsReturn = p.blocks.map(_.jump).find(_.isInstanceOf[Return]).isDefined
    if (toAll && p.blocks.isEmpty && p.entryBlock.isEmpty && p.returnBlock.isEmpty) {
      p.returnBlock = (Block(label=p.name + "_basil_return",jump=Return()))
      p.entryBlock = (Block(label=p.name + "_basil_entry",jump=GoTo(p.returnBlock.get)))
    } else if (p.returnBlock.isEmpty && (toAll || containsReturn)) {
      p.returnBlock = p.addBlocks(Block(label=p.name + "_basil_return",jump=Return()))
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

