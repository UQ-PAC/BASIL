package ir.transforms
import ir.*
import cilvisitor.*

def removeBodyOfExternal(external: Set[String])(prog: Program) = {
  prog.procedures.foreach {
    case p =>
      if (external.contains(p.procName)) {
        // update the modifies set before removing the body
        p.modifies.addAll(p.blocks.flatMap(_.modifies))
        p.replaceBlocks(Seq())
      }
  }
}
