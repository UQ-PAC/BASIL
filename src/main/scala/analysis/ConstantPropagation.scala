package analysis

import analysis.lattices._

object ConstantPropagationAnalysis:

  /** Intraprocedural analysis that uses the worklist solver.
    */
  class WorklistSolver(cfg: ProgramCfg)
      extends ValueAnalysisWorklistSolver(cfg, ConstantPropagationLattice)