package test_util

import boogie.SpecGlobal
import ir.{IRContext, Program, cilvisitor, transforms}
import specification.Specification

def programToContext(
  program: Program,
  globals: Set[SpecGlobal] = Set.empty,
  globalOffsets: Map[BigInt, BigInt] = Map.empty
): IRContext = {
  cilvisitor.visit_prog(transforms.ReplaceReturns(), program)
  transforms.addReturnBlocks(program)
  cilvisitor.visit_prog(transforms.ConvertSingleReturn(), program)

  val spec = Specification(Set(), globals, Map(), List(), List(), List(), Set())
  IRContext(List(), Set(), globals, Set(), globalOffsets, spec, program)
}
