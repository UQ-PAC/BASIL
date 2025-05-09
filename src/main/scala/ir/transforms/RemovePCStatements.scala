package ir.transforms

import ir.*
import ir.cilvisitor.*

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

