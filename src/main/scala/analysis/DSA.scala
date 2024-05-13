package analysis

import ir.{BitVecLiteral, CFGPosition, Procedure, Program, Register, Variable}
import specification.{ExternalFunction, SpecGlobal}

import scala.collection.mutable

class DSA(program: Program,
            symResults: Map[CFGPosition, Map[SymbolicAccess, TwoElement]],
            constProp: Map[CFGPosition, Map[Variable, FlatElement[BitVecLiteral]]],
            globals: Set[SpecGlobal], globalOffsets: Map[BigInt, BigInt],
            externalFunctions: Set[ExternalFunction],
            reachingDefs: Map[CFGPosition, Map[Variable, Set[CFGPosition]]],
            writesTo: Map[Procedure, Set[Register]],
            params:  Map[Procedure, Set[Variable]]
         ) extends Analysis[Any] {

  val DSGs : mutable.Map[Procedure, DSG] = mutable.Map()
  override def analyze(): Any = {
    program.procedures.foreach(
      proc =>
        val dsg = Local(proc, symResults, constProp, globals, globalOffsets, externalFunctions, reachingDefs, writesTo, params).analyze()
        DSGs.update(proc, dsg)
    )
  }
}
