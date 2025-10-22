package ir.transforms

import ir.*
import ir.cilvisitor.*

class CountStatements extends CILVisitor {
  var count = 0

  override def vstmt(s: Statement) = {
    count += 1
    SkipChildren()
  }
}
