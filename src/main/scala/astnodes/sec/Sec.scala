package astnodes.sec
import astnodes.exp.`var`.{MemLoad, Register, Var}

trait Sec {
  def vars: List[Var]
}
