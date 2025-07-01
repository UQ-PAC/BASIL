package ir.parsing

import basil_ir.Absyn as syntax

type TypesVisitorType[A] = syntax.Type.Visitor[ir.IRType, A] & syntax.BoolType.Visitor[ir.IRType, A] &
  syntax.MapType.Visitor[ir.IRType, A] & syntax.IntType.Visitor[ir.IRType, A] & syntax.BVType.Visitor[ir.IRType, A]

trait TypesBNFCVisitor[A]
    extends syntax.Type.Visitor[ir.IRType, A],
      syntax.BoolType.Visitor[ir.IRType, A],
      syntax.MapType.Visitor[ir.IRType, A],
      syntax.IntType.Visitor[ir.IRType, A],
      syntax.BVType.Visitor[ir.IRType, A] {

  import scala.language.implicitConversions

  // Members declared in Type.Visitor
  override def visit(x: syntax.TypeIntType, arg: A) = x.inttype_.accept(this, arg)
  override def visit(x: syntax.TypeBoolType, arg: A) = x.booltype_.accept(this, arg)
  override def visit(x: syntax.TypeMapType, arg: A) = x.maptype_.accept(this, arg)
  override def visit(x: syntax.TypeBVType, arg: A) = x.bvtype_.accept(this, arg)

  // Members declared in BoolType.Visitor
  override def visit(x: syntax.BoolT, arg: A) = ir.BoolType

  // Members declared in MapType.Visitor
  override def visit(x: syntax.MapT, arg: A) =
    ir.MapType(x.type_1.accept(this, arg), x.type_2.accept(this, arg))

  // Members declared in IntType.Visitor
  override def visit(x: syntax.IntT, arg: A) = ir.IntType

  // Members declared in BVType.Visitor
  override def visit(x: syntax.BVT, arg: A) = ir.BitVecType(x.bvtype_.stripPrefix("bv").toInt)
}

private def ensureTraitHasNoAbstractMembers[A] = new TypesBNFCVisitor[A] {}
