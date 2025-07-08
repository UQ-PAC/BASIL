package analysis

import ir.{DirectCall, GlobalVar, LocalAssign, MemoryLoad, Procedure, Program, Register}

import scala.collection.mutable

class WriteToAnalysis(program: Program) extends Analysis[Map[Procedure, Set[GlobalVar]]] {

  val writesTo: mutable.Map[Procedure, Set[GlobalVar]] = mutable.Map()
  val mallocGlobalVar = Register("R0", 64)
  val paramGlobalVars: Set[GlobalVar] = Set(
    mallocGlobalVar,
    Register("R1", 64),
    Register("R2", 64),
    Register("R3", 64),
    Register("R4", 64),
    Register("R5", 64),
    Register("R6", 64),
    Register("R7", 64)
  )

  def getWritesTos(proc: Procedure): Set[GlobalVar] = {
    if writesTo.contains(proc) then writesTo(proc)
    else
      val writtenTo: mutable.Set[GlobalVar] = mutable.Set()
      proc.blocks.foreach { block =>
        block.statements.foreach {
          case LocalAssign(variable: GlobalVar, _, _) if paramGlobalVars.contains(variable) =>
            writtenTo.add(variable)
          case MemoryLoad(lhs: GlobalVar, _, _, _, _, _) if paramGlobalVars.contains(lhs) =>
            writtenTo.add(lhs)
          case DirectCall(target, _, _, _) if target.procName == "malloc" =>
            writtenTo.add(mallocGlobalVar)
          case d: DirectCall if program.procedures.contains(d.target) =>
            writtenTo.addAll(getWritesTos(d.target))
          case _ =>
        }
      }

      writesTo.update(proc, writtenTo.toSet)
      writesTo(proc)
  }

  def analyze(): Map[Procedure, Set[GlobalVar]] =
    program.procedures.foreach(getWritesTos)
    writesTo.toMap
}
