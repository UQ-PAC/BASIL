package astnodes.sec
import astnodes.exp.variable.{MemLoad, Register, Variable}

trait Sec {
  def vars: List[Variable]
}
