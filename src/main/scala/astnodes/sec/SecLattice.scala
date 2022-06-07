package astnodes.sec

import vcgen.State

// TODO add checks to make sure attackerLevel is in elements
case class SecLattice(elements: List[SecLevelDefn], attackerLevel: SecVar, bottom: SecVar, top: SecVar) {
  override def toString: String = "type SecurityLevel;\n" + elements.mkString("\n") + s"\n axiom (forall x: SecurityLevel :: $bottom <: x);"
}

case class SecLevelDefn(name: String, parents: List[String]) {
  override def toString = s"const unique s_$name: SecurityLevel extends ${parents.map(x => s"s_$x").mkString(", ")} complete;"
}

case object SecLattice {
  val booleanLattice: SecLattice = SecLattice(List(SecLevelDefn("FALSE", List()), SecLevelDefn("TRUE", List("FALSE"))), SecVar("TRUE"), SecVar("TRUE"), SecVar("FALSE"))
  val TRUE: SecVar = SecVar("TRUE")
  val FALSE: SecVar = SecVar("FALSE")
}


extension (secs: List[Sec]) {
  def join(state: State): Sec = secs match {
    case sec :: Nil => sec
    case sec :: rest => SecBinOp("join", sec, rest.join(state))
    case Nil => state.lattice.attackerLevel
  }
}
