package analysis
import ir.transforms.ReadWriteAnalysis.*
import ir.transforms.ReadWriteAnalysis

import ir.{DirectCall, LocalAssign, MemoryLoad, MemoryStore, Procedure, Program, Register}

import scala.collection.mutable

class WriteToAnalysis(program: Program) extends Analysis[Map[Procedure, Set[Register]]] {

  lazy val result = ir.transforms.ReadWriteAnalysis.readWriteSets(program)

  val paramRegisters = ((0 to 7).toSet).map(i => Register(s"R${i}", 64)).toSet
  val overApprox = ((0 to 31).toSet -- (19 to 28).toSet).map(i => Register(s"R${i}", 64)).toSet

  def getWritesTos(proc: Procedure): Set[Register] = {
    result
      .get(proc)
      .map {
        case Some(r) =>
          r.writes.collect { case reg: Register =>
            reg
          }.toSet
        case None => overApprox
      }
      .toSet
      .flatten
  }

  def analyze(): Map[Procedure, Set[Register]] =
    result.keySet.map(p => p -> getWritesTos(p)).toMap
}
