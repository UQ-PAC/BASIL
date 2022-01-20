package astnodes.pred.secLattice

// TODO add checks to make sure attackerLevel is in elements
case class SecurityLattice(elements: List[SecurityLatticeElement], attackerLevel: String) {
  override def toString = "type SecurityLevel;\n" + elements.mkString("\n")
}

case class SecurityLatticeElement(name: String, parents: List[String]) {
  override def toString = s"const unique s_$name: SecurityLevel extends ${parents.map(x => s"s_$x").mkString(", ")} complete;"
}

case object SecurityLattice {
  val booleanLattice = SecurityLattice(List(SecurityLatticeElement("false", List()), SecurityLatticeElement("true", List("false"))), "true")
}
