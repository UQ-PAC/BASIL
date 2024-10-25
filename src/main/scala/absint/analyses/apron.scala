package absint.analyses
import absint.*
import ir.*
import analysis.{RegisterVariableWrapper, getDefinition, getUse, RegisterWrapperEqualSets}
import scala.collection.immutable.HashSet
import scala.collection.mutable
import scala.util.Random
import apron.*


def littoscalar(e : Literal) = {
  e match {
    case BitVecLiteral(v, sz) => MpqScalar(v.bigInteger)
    case IntLiteral(l) => MpqScalar(l.bigInteger)
    case TrueLiteral => MpqScalar(0)
    case FalseLiteral => MpqScalar(1)
  }
}

def toScalar(e: Expr) = {
  e match {
    case l: Literal => littoscalar(l)
    case _ => ???
  }
}

def expToTexpr1(e: Expr) = {
  e match {
    case b: BinaryExpr => binexpToTexp1(b)
    case l: Literal => Texpr1CstNode(littoscalar(l))
    case v : Variable => Texpr1VarNode(StringVar(v.name))
    case _ => ???
  }

}

def binexpToTexp1(e: BinaryExpr) : Texpr1Node = {
  val lhs = expToTexpr1(e.arg1)
  val rhs = expToTexpr1(e.arg2)
  (e.getType, e.op) match {
    case (BitVecType(sz), BVADD) => Texpr1BinNode(Texpr1BinNode.OP_ADD, lhs, rhs)
    case (BitVecType(sz), BVSUB) => Texpr1BinNode(Texpr1BinNode.OP_SUB, lhs, rhs)
    case (BitVecType(sz), BVMUL) => Texpr1BinNode(Texpr1BinNode.OP_MUL, lhs, rhs)
    case (BitVecType(sz), BVSDIV) => Texpr1BinNode(Texpr1BinNode.OP_DIV, lhs, rhs)
    case (BitVecType(sz), BVCOMP) => Texpr1BinNode(Texpr1BinNode.OP_DIV, lhs, rhs)
    case _ => ???
  }
}

def binexpToCons(env: Environment, e: BinaryExpr) : Tcons1 = {
  val lhs = expToTexpr1(e.arg1)
  val rhs = toScalar(e.arg2)
  e match {
    case BinaryExpr(BVEQ, BinaryExpr(BVCOMP, x, y), BitVecLiteral(1, _)) => Tcons1(env, Tcons1.EQ, lhs, rhs)
    case BinaryExpr(BVEQ, BinaryExpr(BVCOMP, x, y), BitVecLiteral(0, _)) => Tcons1(env, Tcons1.DISEQ, lhs, rhs)
  }

}


def expToCons(env: Environment, e: Expr) : Tcons1 = {
  e match {
    case b: BinaryExpr => binexpToCons(env, b)
    case _ => ???
  }
}

def xfer(env: Environment, man: Manager, p: Abstract1, s: Statement) : Abstract1 = {
  s match {
    case a: Assign => {
      p.assignCopy(man, Array[Var](StringVar(a.lhs.name)), Array[Texpr1Intern](Texpr1Intern(env, expToTexpr1(a.rhs))), p)
    }
    case a : Assume => {
      val cond = expToCons(env, a.body)
      p.meet(man, cond)
      p
    }
    case a: Assert => {
      val cond = expToCons(env, a.body)
      println("assert " + a.body + " : " + p.satisfy(man, cond))
      p
    }
  }


}

def testApron = {
  val octman = Octagon()
  val (vx, vy, vz) = (StringVar("x"), StringVar("y"), StringVar("z"))
  val env = Environment(Array[Var](), Array[Var](vx, vy, vz))
  // val linman = LinCons1(env)
  println(env)

  val c = Linexpr1(env)
  c.setCoeff(vx, DoubleScalar(-1.0))
  c.setCoeff(vy, DoubleScalar(2.0))
  println(c)



}
