package astnodes.pred.secLattice

import astnodes.pred.secLattice.SecurityLatticeElement

case class SecLatticeBinOp(op: String, first: SecurityLatticeElement, second: SecurityLatticeElement) {
  override def toString = s"$op($first, $second)"
}

