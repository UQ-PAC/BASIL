package absint.analyses
import absint.*
import ir.*
import analysis.{RegisterVariableWrapper, getDefinition, getUse, RegisterWrapperEqualSets}
import scala.collection.immutable.HashSet
import scala.util.Random

case class CopyProp(expr: Expr, deps: Set[RegisterVariableWrapper])

case class CCP(
    constants: Map[RegisterVariableWrapper, Literal],
    // variable -> expr * dependencies
    val exprs: Map[RegisterVariableWrapper, CopyProp]
)

class ConstCopyProp(val reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])])
    extends AbstractDomain[CCP] {

  private final val callClobbers = (0 to 30).map("R" + _).map(c => Register(c, 64))

  def top: CCP = CCP(Map(), Map())
  def bot: CCP = CCP(Map(), Map())

  override def join(l: CCP, r: CCP): CCP = {
    CCP(
      (l.constants ++ r.constants)
        .removedAll(l.constants.keySet.intersect(r.constants.keySet).filterNot(k => l.constants(k) == r.constants(k))),
      (l.exprs ++ r.exprs).removedAll(l.exprs.keySet.intersect(r.exprs.keySet).filterNot(k => l.exprs(k) == r.exprs(k)))
    )
  }

  override def transfer(c: CCP, s: Statement): CCP = {
    s match {
      case Assign(lv, r, lb) if r.loads.isEmpty => {
        val l = RegisterVariableWrapper(lv, getDefinition(lv, s, reachingDefs))

        var p = c
        val evaled = ir.eval.simplifyExpression(
          eval.partialEvalExpr(
            ir.eval.simplifyExpression(r),
            v => p.constants.get(RegisterVariableWrapper(v, getUse(v, s, reachingDefs)))
          )
        )
        val rhsDeps = evaled.variables.map(v => RegisterVariableWrapper(v, getUse(v, s, reachingDefs)))

        p = evaled match {
          case lit: Literal => p.copy(constants = p.constants.updated(l, lit))
          case _: Expr =>
            p.copy(constants = p.constants.removed(l), exprs = p.exprs.updated(l, CopyProp(evaled, rhsDeps)))
        }

        // remove candidates whose value changes due to this update
        // without an SSA form in the output, we can't propagate assignments such that R0_1 := f(R0_0)
        //  or; only replace such that all uses are copyproped, the dead definition is removed
        p = p.copy(exprs = p.exprs.filterNot((k, v) => v.deps.exists(c => c.variable.name == l.variable.name)))

        p
      }
      case x: Call => {
        val toClob = callClobbers.map(v => RegisterVariableWrapper(v, getDefinition(v, s, reachingDefs)))
        toClob.foldLeft(c)((c, l) =>
          c.copy(constants = c.constants.removed(l), exprs = c.exprs.filterNot((k, v) => v.deps.contains(l)))
        )
      }
      case _ => c
    }
  }
}

// class RegularConstProp extends AbstractMapDomain[Variable, CompleteValueLattice[Literal]](ConstantDomain()) {
//
// }

class ConstProp extends AbstractMapDomain[
      Variable,
      CompleteValueLattice[Literal],
    ](ConstantDomain[Literal]()) {

  val callClobbers = (0 to 30).map("R" + _).map(c => Register(c, 64))

  override def transfer(x: ML[Variable, CompleteValueLattice[Literal]], s: Statement) = {
    s match {
      case Assign(l, r, lb) => {

        val evaled = eval.partialEvalExpr(r,
          (v => x(v) match {
              case Val(v) => Some(v)
              case _      => None
          })
        )

        evaled match {
          case lit: Literal => x.updated(l, d.join(x(l), Val(lit)))
          case _            => x.updated(l, Top)
        }
      }
      // case c: Call =>  callClobbers.foldLeft(x)((v, l) => v.updated(l, Top))
       case _ => x
    }
  }
}

class SSAConstProp(val reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])])
    extends AbstractMapDomain[
      RegisterWrapperEqualSets,
      ValueSet[CompleteValueLattice[Literal]],
    ](BoundedPowerSetDomain(ConstantDomain[Literal](), Some(8))) {

  val callClobbers = (0 to 30).map("R" + _).map(c => Register(c, 64))

  override def transfer(
      c: ML[RegisterWrapperEqualSets, ValueSet[CompleteValueLattice[Literal]]],
      s: Statement
  ): ML[RegisterWrapperEqualSets, ValueSet[CompleteValueLattice[Literal]]] = {
    s match {
      case Assign(lv, r, lb) if r.loads.isEmpty => {
        val l = RegisterWrapperEqualSets(lv, getDefinition(lv, s, reachingDefs))

        var p = c
        val evaled = ir.eval.simplifyExpression(
          eval.partialEvalExpr(
            ir.eval.simplifyExpression(r),
            v =>
              p.get(RegisterWrapperEqualSets(v, getUse(v, s, reachingDefs))).flatMap {
                case ValueSet.Elem(v) =>
                  val es = v.collect { case Val(v) =>
                    v
                  }.toSeq
                  if (es.size > 0) then Some(es(Random.nextInt(es.size))) else None
                case _ => None
              }
          )
        )

        evaled match {
          case lit: Literal => p.updated(l, p(l) + Val(lit))
          case _            => p.updated(l, p(l) + Top)
        }
      }
      case x: Call => {
        // use getDefinition (not getUse) because this statement does not define everything or use anything
        val toClob = callClobbers.map(v => RegisterWrapperEqualSets(v, getDefinition(v, s, reachingDefs)))
        toClob.foldLeft(c)((c, l) => c.removed(l))
      }
      case _ => c
    }
  }

  def toMapL(v: ML[RegisterWrapperEqualSets, CompleteValueLattice[Literal]]): Map[RegisterWrapperEqualSets, Literal] = {
    val vd = ConstantDomain[Literal]()
    v.toMap.collect { case (k, Val(v)) =>
      (k, v)
    }
  }
}

// def powersetCP(reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])]) = BoundedSetDomain(SSAConstProp(reachingDefs), Some(2))


def parAnalyseConstProp(p: Program) : Map[CFGPosition, Map[Variable, analysis.FlatElement[BitVecLiteral]]] = {
  val domain = CollectingDomain(ConstProp())
  val cd = ConstantDomain()
  val solver = worklistSolver(domain)
  val res : Map[CFGPosition, ML[Variable, CompleteValueLattice[Literal]]] = (solver.parSolveProg(p, Set(), Set())).toList.map(k => domain.toMap(k._2)).flatten.toMap
  val res2 : Map[CFGPosition, Map[Variable, CompleteValueLattice[Literal]]] = res.map(k => (k._1, k._2.toMap))
  val res3 = res2.map(k => (k._1, k._2.collect {
    case (k, Val(v: BitVecLiteral)) => (k, analysis.FlatEl(v))
  }.withDefaultValue(analysis.Top))).withDefaultValue(Map().withDefaultValue(analysis.Top))

  res3
}

def analyseConstProp(p: Program) = {
  val domain = CollectingDomain(ConstProp())
  val solver = worklistSolver(domain)
  val res : List[Map[CFGPosition, ML[Variable, CompleteValueLattice[Literal]]]] = (solver.solveProg(p, Set(), Set())).toList.map(k => domain.toMap(k._2))
  res.flatten.toMap
}

def analyseSSAConstProp(
    p: Program,
    reachingDefs: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])]
): Map[CFGPosition, Map[RegisterWrapperEqualSets, Set[BitVecLiteral]]] = {
  val domain = SSAConstProp(reachingDefs)
  val solver = worklistSolver(domain)
  val res = (solver.solveProg(p, Set(), Set())).map(_._2)

  def convel[V](v: ValueSet[CompleteValueLattice[Literal]]): Set[BitVecLiteral] = {
    v match {
      case ValueSet.Elem(e) =>
        e.collect { 
          case Val(b: BitVecLiteral) => b
        }
      case _ => Set()
    }
  }

  val fl = (res.reduce((l, r) => (l.merge(r)))).toMap
  val flatelmap = fl.map(k => (k._1, (convel(k._2)))).withDefaultValue(Set())
  Map().withDefaultValue(flatelmap)
}

class LatticeDomain[L, LD <: analysis.Lattice[L]](lattice: LD) extends AbstractDomain[L] {
  override def bot = lattice.bottom
  override def top = lattice.top
  override def join(a: L, b: L): L = lattice.lub(a, b)
  override def transfer(x: L, s: Statement) = x
}

class CPClass(rd: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])])
    extends analysis.ConstantPropagationWithSSA(rd)

//class OSSAConstProp(val rd: Map[CFGPosition, (Map[Variable, Set[Assign]], Map[Variable, Set[Assign]])])
//    extends LatticeDomain(analysis.ConstantPropagationLatticeWithSSA()) {
//  val cpanalysis = CPClass(rd)
//  override def transfer(v: Map[analysis.RegisterWrapperEqualSets, Set[BitVecLiteral]], s: Statement) =
//    cpanalysis.localTransfer(s, v)
//
//}
