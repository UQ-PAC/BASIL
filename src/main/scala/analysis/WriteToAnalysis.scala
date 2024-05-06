package analysis

import ir.{Assert, Assume, BitVecType, Call, DirectCall, GoTo, LocalAssign, MemoryAssign, NOP, Procedure, Program, Register}

import scala.collection.mutable

class WriteToAnalysis(program: Program) extends Analysis[Map[Procedure, Set[Register]]] {

  val writesTo: mutable.Map[Procedure, Set[Register]] = mutable.Map()
  val mallocRegister = Register("R0", BitVecType(64))
  val paramRegisters: Set[Register] = Set(
    mallocRegister,
    Register("R1", BitVecType(64)),
    Register("R2", BitVecType(64)),
    Register("R3", BitVecType(64)),
    Register("R4", BitVecType(64)),
    Register("R5", BitVecType(64)),
    Register("R6", BitVecType(64)),
    Register("R7", BitVecType(64)),
  )

  def getWritesTos(proc: Procedure): Set[Register] = {
    if writesTo.contains(proc) then
      writesTo(proc)
    else
      val writtenTo : mutable.Set[Register] = mutable.Set()
      proc.blocks.foreach(
        block =>
          block.statements.foreach {
            case LocalAssign(variable: Register, value, label) if paramRegisters.contains(variable) =>
              writtenTo.add(variable)
            case _ =>
          }

          block.jump match
            case DirectCall(proc, returnTarget, label) if proc.name == "malloc" =>
              writtenTo.add(mallocRegister)
            case DirectCall(proc, returnTarget, label) if program.procedures.contains(proc) =>
              writtenTo.++=(getWritesTos(proc))
            case _ =>
      )

      writesTo.update(proc, writtenTo.toSet)
      writesTo(proc)
  }

  def analyze(): Map[Procedure, Set[Register]] =
    program.procedures.foreach(getWritesTos)
    writesTo.toMap
}
