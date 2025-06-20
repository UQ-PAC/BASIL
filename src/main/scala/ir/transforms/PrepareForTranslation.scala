package ir.transforms

import scala.collection.mutable
import ir.{StackSubstituter, Renamer, Block}
import ir.invariant
import util.BASILConfig
import analysis.AnalysisManager

// run iff arg.isEmpty || (arg.get.memoryRegions == MemoryRegionsMode.Disabled)
val determineRelevantMemory = SingleTransform("DetermineRelevantMemory", (ctx, man) => {
  ctx.program.determineRelevantMemory(ctx.globalOffsets)
  man.ClobberAll
})

// run iff arg
val stackSubstitution = SingleTransform("StackSubstitution", (ctx, man) => {
  StackSubstituter().visitProgram(ctx.program)
  man.ClobberAll
})

val setModifies = SingleTransform("SetModifies", (ctx, man) => {
  val specModifies = ctx.specification.subroutines.map(s => s.name -> s.modifies).toMap
  ctx.program.setModifies(specModifies)
  man.ClobberAll
})

def getRenameBoogieKeywordsTransform(boogieReserved: Set[String]): Transform =
  SingleTransform("RenameBoogieKeywords", (ctx, man) => {
    Renamer(boogieReserved).visitProgram(ctx.program)
    man.ClobberAll
  })

/** Cull unneccessary information that does not need to be included in the translation, and infer stack regions, and
  * add in modifies from the spec.
  */
def getPrepareForTranslationTransform(config: BASILConfig, boogieReserved: Set[String]): Transform = TransformBatch(
  "PrepareForTranslation",
  List(
    determineRelevantMemory, // run iff config.staticAnalysis.isEmpty || (config.staticAnalysis.get.memoryRegions == MemoryRegionsMode.Disabled)
    getStripUnreachableFunctionsTransform(config.loading.procedureTrimDepth),
    stackSubstitution, // run iff !config.memoryTransform && (config.staticAnalysis.isEmpty || (config.staticAnalysis.get.memoryRegions == MemoryRegionsMode.Disabled))
    setModifies,
    getRenameBoogieKeywordsTransform(boogieReserved: Set[String])
  ),
  postRunChecks = ctx => {
    assert(invariant.singleCallBlockEnd(ctx.program))
    // check all blocks with an atomic section exist within the same procedure
    val visited = mutable.Set[Block]()
    for (p <- ctx.program.procedures) {
      for (b <- p.blocks) {
        if (!visited.contains(b)) {
          if (b.atomicSection.isDefined) {
            b.atomicSection.get.getBlocks.foreach { a => assert(a.parent == p) }
            visited.addAll(b.atomicSection.get.getBlocks)
          }
          visited.addOne(b)
        }
      }
    }
  }
)
