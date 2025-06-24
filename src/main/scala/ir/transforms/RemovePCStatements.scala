package ir.transforms

import ir._
import ir.cilvisitor._

class RemovePCStatements extends CILVisitor {
  override def vstmt(s: Statement) = {
    val vars = allVarsPos(s)
    if (vars.contains(Register("_PC", 64))) {
      ChangeTo(List())
    } else {
      SkipChildren()
    }
  }
}
