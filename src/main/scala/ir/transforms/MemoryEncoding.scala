package ir.transforms

import boogie.*
import ir.cilvisitor.*
import ir.*

class MemoryEncodingTransform() extends CILVisitor {
  private def transform_malloc(p: Procedure) = {
    p.requires = p.requires ++ List(
      // Dummy for my sanity
      BinaryBExpr(EQ, BitVecBLiteral(121, 64), BitVecBLiteral(121, 64))
    )
  }

  override def vproc(p: Procedure) = {
    p.procName match {
      case "malloc" => transform_malloc(p)
      case _ => { }
    }

    SkipChildren()
  }
}
