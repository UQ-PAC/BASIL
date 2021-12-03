package analysis

import astnodes.stmt.Stmt

trait Analysis {
  def transfer(stmt: Stmt, elem: LatticeElement): LatticeElement;
}
