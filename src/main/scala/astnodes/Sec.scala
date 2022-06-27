/*
package astnodes

//import vcgen.State


trait Sec {
  //def vars: List[Variable]
}

case class SecBinOp(op: String, first: Sec, second: Sec) extends Sec {
  override def toString = s"$op($first, $second)"
  //override def vars: List[Variable] = first.vars ++ second.vars
}

// TODO not used
case class SecITE(cond: Pred, first: Sec, second: Sec) extends Sec {
  override def toString = s"secITE($cond, $first, $second)"
  //override def vars: List[Variable] = cond.vars ++ first.vars ++ second.vars
}

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

/*
extension (secs: List[Sec]) {
  def join(state: State): Sec = secs match {
    case sec :: Nil => sec
    case sec :: rest => SecBinOp("join", sec, rest.join(state))
    case Nil => state.lattice.attackerLevel
  }
}
 */

case class SecMemLoad (gamma: Boolean, L: Boolean, loc: Expr) extends Sec {
  override def toString: String =
    if (L) s"L(${loc.toBoogieString}, heap)"
    else String.format("%s%s[%s]", if (this.gamma) "Gamma_" else "", if (this.onStack) "stack" else "heap", loc.toBoogieString)

  def onStack: Boolean = loc match {
    // TODO improve
    case v: LocalVar => v.name == "R31"
    case BinOp(_, v: LocalVar, _) => v.name == "R31"
    case _ => false
  }

  //override def vars: List[Variable] = List()
}

/**
 * if gamma = true then this is a security level variable
 * else it is a security level literal
 */
case class SecVar (name: String, gamma: Boolean = false) extends Sec {
  override def toString: String = s"${if (gamma) "Gamma" else "s"}_$name"
  //override def vars: List[Variable] = List()
}
 */
