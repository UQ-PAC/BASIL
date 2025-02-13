package analysis

import ir.{DirectCall, LocalAssign, MemoryLoad, MemoryStore, Procedure, Program, Register}

import scala.collection.mutable

class WriteToAnalysis(program: Program) extends Analysis[Map[Procedure, Set[Register]]] {

  val writesTo: mutable.Map[Procedure, Set[Register]] = mutable.Map()
  val mallocRegister = Register("R0", 64)
  val paramRegisters: Set[Register] = Set(
    mallocRegister,
    Register("R1", 64),
    Register("R2", 64),
    Register("R3", 64),
    Register("R4", 64),
    Register("R5", 64),
    Register("R6", 64),
    Register("R7", 64),
  )

  def getWritesTos(proc: Procedure): Set[Register] = {
    if writesTo.contains(proc) then
      writesTo(proc)
    else
      val writtenTo: mutable.Set[Register] = mutable.Set()
      proc.blocks.foreach { block =>
        block.statements.foreach {
          case LocalAssign(variable: Register, _, _) if paramRegisters.contains(variable) =>
            writtenTo.add(variable)
          case MemoryLoad(lhs: Register, _, _, _, _, _) if paramRegisters.contains(lhs) =>
            writtenTo.add(lhs)
          case DirectCall(target, _, _, _) if target.name == "malloc" =>
            writtenTo.add(mallocRegister)
          case d: DirectCall if program.procedures.contains(d.target) =>
            writtenTo.addAll(getWritesTos(d.target))
          case _ =>
        }
    }

      writesTo.update(proc, writtenTo.toSet)
      writesTo(proc)
  }

  def analyze(): Map[Procedure, Set[Register]] =
    program.procedures.foreach(getWritesTos)
    writesTo.toMap
}
