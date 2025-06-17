package ir.invariant
import util.Logger
import ir.{Program, Procedure, Return}

def procedureDiamondForm(p: Procedure): Boolean = {

  val stub = p.blocks.isEmpty || p.isExternal.contains(true)
  val entryOK = p.entryBlock.exists(_.prevBlocks.isEmpty)
  val retOK = p.returnBlock.forall(_.jump.isInstanceOf[Return])
  val returning =
    p.blocks.filterNot(p.entryBlock.contains).filterNot(p.returnBlock.contains).filter(_.jump.isInstanceOf[Return])

  if (!stub && !entryOK) {
    Logger.error(
      s"${p.name} Entry block malformed, missing or has incoming jumps: ${p.entryBlock.map(_.incomingJumps)}"
    )
  }
  if (!stub && !retOK) {
    Logger.error(s"${p.name} Return block malformed, ${p.returnBlock.map(translating.PrettyPrinter.pp_block)}")
    println(translating.PrettyPrinter.pp_proc(p))
  }
  if (returning.nonEmpty) {
    Logger.error(s"${p.name} Return block non-exclusie return: ${returning.mkString(", ")}")
  }
  stub || (entryOK && retOK && returning.isEmpty)
}

def programDiamondForm(p: Program): Boolean = {
  p.procedures.forall(procedureDiamondForm)
}
