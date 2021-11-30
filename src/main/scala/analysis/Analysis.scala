package analysis

import facts.stmt.Stmt

trait Analysis {
  def transfer(stmt: Stmt, elem: LatticeElement): LatticeElement;
}