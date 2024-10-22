package boogie

import ir.{Endian, IntBinOp, IntUnOp}
import specification.{ArrayAccess, SpecGamma, SpecGlobal}

trait BVisitor {
  def visitBExpr(node: BExpr): BExpr = node.acceptVisit(this)

  def visitIntBLiteral(node: IntBLiteral): BExpr = node

  def visitBVExtract(node: BVExtract): BExpr = node.copy(body = visitBExpr(node.body))

  def visitBVRepeat(node: BVRepeat): BExpr = node.copy(body = visitBExpr(node.body))

  def visitBVZeroExtend(node: BVZeroExtend): BExpr = node.copy(body = visitBExpr(node.body))

  def visitBVSignExtend(node: BVSignExtend): BExpr = node.copy(body = visitBExpr(node.body))

  def visitBFunctionCall(node: BFunctionCall): BExpr = node.copy(args = node.args.map(visitBExpr))

  def visitUnaryBExpr(node: UnaryBExpr): BExpr = node.copy(arg = visitBExpr(node.arg))

  def visitBinaryBExpr(node: BinaryBExpr): BExpr = {
    node.copy(arg1 = visitBExpr(node.arg1), arg2 = visitBExpr(node.arg2))
  }

  def visitIfThenElse(node: IfThenElse): BExpr = {
    node.copy(guard = visitBExpr(node.guard), thenExpr = visitBExpr(node.thenExpr), elseExpr = visitBExpr(node.elseExpr))
  }

  def visitOld(node: Old): BExpr = node.copy(body = visitBExpr(node.body))

  def visitSpecGlobal(node: SpecGlobal): BExpr = node

  def visitSpecGamma(node: SpecGamma): BExpr = node

  def visitArrayAccess(node: ArrayAccess): BExpr = node
}

trait SpecResolutionVisitor extends BVisitor {
  override def visitUnaryBExpr(node: UnaryBExpr): BExpr = {
    node.op match {
      case i: IntUnOp => node.copy(op = i.toBV, arg = visitBExpr(node.arg))
      case _ => node.copy(arg = visitBExpr(node.arg))
    }
  }

  override def visitBinaryBExpr(node: BinaryBExpr): BExpr = {
    node.op match {
      case i: IntBinOp => node.copy(op = i.toBV, arg1 = visitBExpr(node.arg1), arg2 = visitBExpr(node.arg2))
      case _ => node.copy(arg1 = visitBExpr(node.arg1), arg2 = visitBExpr(node.arg2))
    }
  }
}

object ResolveSpec extends SpecResolutionVisitor {
  override def visitSpecGlobal(node: SpecGlobal): BMemoryLoad = {
    BMemoryLoad(
      BMapVar("mem", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Global),
      node.toAddrVar,
      Endian.LittleEndian,
      node.size
    )
  }

  override def visitSpecGamma(node: SpecGamma): GammaLoad = {
    GammaLoad(
      BMapVar("Gamma_mem", MapBType(BitVecBType(64), BoolBType), Scope.Global),
      node.global.toAddrVar,
      node.global.size,
      node.global.size / 8
    )
  }

  override def visitArrayAccess(node: ArrayAccess): BMemoryLoad = {
    BMemoryLoad(
      BMapVar("mem", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Global),
      node.toAddrVar,
      Endian.LittleEndian,
      node.global.size
    )
  }

}

object ResolveOld extends SpecResolutionVisitor {
  override def visitOld(node: Old): BExpr = ResolveInsideOld.visitBExpr(node.body)
  override def visitSpecGlobal(node: SpecGlobal): BMemoryLoad = ResolveSpec.visitSpecGlobal(node)
  override def visitSpecGamma(node: SpecGamma): GammaLoad = ResolveSpec.visitSpecGamma(node)
  override def visitArrayAccess(node: ArrayAccess): BMemoryLoad = ResolveSpec.visitArrayAccess(node)
}

object RemoveOld extends SpecResolutionVisitor {
  override def visitOld(node: Old): BExpr = ResolveSpec.visitBExpr(node.body)
  override def visitSpecGlobal(node: SpecGlobal): BMemoryLoad = ResolveSpec.visitSpecGlobal(node)
  override def visitSpecGamma(node: SpecGamma): GammaLoad = ResolveSpec.visitSpecGamma(node)
  override def visitArrayAccess(node: ArrayAccess): BMemoryLoad = ResolveSpec.visitArrayAccess(node)
}

object ResolveSpecL extends SpecResolutionVisitor {
  override def visitSpecGlobal(node: SpecGlobal): BMemoryLoad = {
    BMemoryLoad(
      BMapVar("memory", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Parameter),
      node.toAddrVar,
      Endian.LittleEndian,
      node.size
    )
  }

  override def visitSpecGamma(node: SpecGamma): GammaLoad = ResolveSpec.visitSpecGamma(node)

  override def visitArrayAccess(node: ArrayAccess): BMemoryLoad = {
    BMemoryLoad(
      BMapVar("memory", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Parameter),
      node.toAddrVar,
      Endian.LittleEndian,
      node.global.size
    )
  }
}

object ResolveInsideOld extends SpecResolutionVisitor {
  override def visitSpecGlobal(node: SpecGlobal): BExpr = node.toOldVar
  override def visitSpecGamma(node: SpecGamma): BExpr = node.global.toOldGamma
  override def visitArrayAccess(node: ArrayAccess): BExpr = node.toOldVar
}

object ResolveSpecParam extends SpecResolutionVisitor {
  override def visitOld(node: Old): BExpr = ResolveSpecParamOld.visitBExpr(node.body)

  override def visitSpecGlobal(node: SpecGlobal): BMemoryLoad = {
    BMemoryLoad(
      BMapVar("mem$out", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Parameter),
      node.toAddrVar,
      Endian.LittleEndian,
      node.size
    )
  }

  override def visitSpecGamma(node: SpecGamma): GammaLoad = {
    GammaLoad(
      BMapVar("Gamma_mem$out", MapBType(BitVecBType(64), BoolBType), Scope.Parameter),
      node.global.toAddrVar,
      node.global.size,
      node.global.size / 8
    )
  }

  override def visitArrayAccess(node: ArrayAccess): BMemoryLoad = {
    BMemoryLoad(
      BMapVar("mem$out", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Parameter),
      node.toAddrVar,
      Endian.LittleEndian,
      node.global.size
    )
  }
}

object ResolveSpecParamOld extends SpecResolutionVisitor {
  override def visitSpecGlobal(node: SpecGlobal): BMemoryLoad = {
    BMemoryLoad(
      BMapVar("mem$in", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Parameter),
      node.toAddrVar,
      Endian.LittleEndian,
      node.size
    )
  }

  override def visitSpecGamma(node: SpecGamma): GammaLoad = {
    GammaLoad(
      BMapVar("Gamma_mem$in", MapBType(BitVecBType(64), BoolBType), Scope.Parameter),
      node.global.toAddrVar,
      node.global.size,
      node.global.size / 8
    )
  }

  override def visitArrayAccess(node: ArrayAccess): BMemoryLoad = {
    BMemoryLoad(
      BMapVar("mem$in", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Parameter),
      node.toAddrVar,
      Endian.LittleEndian,
      node.global.size
    )
  }
}

object ResolveSpecInv extends SpecResolutionVisitor {
  override def visitOld(node: Old): BExpr = ResolveSpecInvOld.visitBExpr(node.body)

  override def visitSpecGlobal(node: SpecGlobal): BMemoryLoad = {
    BMemoryLoad(
      BMapVar("mem$inv2", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Local),
      node.toAddrVar,
      Endian.LittleEndian,
      node.size
    )
  }

  override def visitSpecGamma(node: SpecGamma): GammaLoad = {
    GammaLoad(
      BMapVar("Gamma_mem$inv2", MapBType(BitVecBType(64), BoolBType), Scope.Local),
      node.global.toAddrVar,
      node.global.size,
      node.global.size / 8
    )
  }

  override def visitArrayAccess(node: ArrayAccess): BMemoryLoad = {
    BMemoryLoad(
      BMapVar("mem$inv2", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Local),
      node.toAddrVar,
      Endian.LittleEndian,
      node.global.size
    )
  }
}

object ResolveSpecInvOld extends SpecResolutionVisitor {
  override def visitSpecGlobal(node: SpecGlobal): BMemoryLoad = {
    BMemoryLoad(
      BMapVar("mem$inv1", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Local),
      node.toAddrVar,
      Endian.LittleEndian,
      node.size
    )
  }

  override def visitSpecGamma(node: SpecGamma): GammaLoad = {
    GammaLoad(
      BMapVar("Gamma_mem$inv1", MapBType(BitVecBType(64), BoolBType), Scope.Local),
      node.global.toAddrVar,
      node.global.size,
      node.global.size / 8
    )
  }

  override def visitArrayAccess(node: ArrayAccess): BMemoryLoad = {
    BMemoryLoad(
      BMapVar("mem$inv1", MapBType(BitVecBType(64), BitVecBType(8)), Scope.Local),
      node.toAddrVar,
      Endian.LittleEndian,
      node.global.size
    )
  }

}