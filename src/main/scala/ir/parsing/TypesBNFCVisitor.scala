package ir.parsing

import basil_ir.Absyn as syntax

/**
 * Parses types into Basil IR types.
 */
trait TypesBNFCVisitor[A]
    extends syntax.Type.Visitor[ir.IRType, A],
      syntax.BoolType.Visitor[ir.IRType, A],
      syntax.MapType.Visitor[ir.IRType, A],
      syntax.IntType.Visitor[ir.IRType, A],
      syntax.BVType.Visitor[ir.IRType, A] {

  // Members declared in Type.Visitor
  override def visit(x: syntax.TypeIntType, arg: A) = x.inttype_.accept(this, arg)
  override def visit(x: syntax.TypeBoolType, arg: A) = x.booltype_.accept(this, arg)
  override def visit(x: syntax.TypeMapType, arg: A) = x.maptype_.accept(this, arg)
  override def visit(x: syntax.TypeBVType, arg: A) = x.bvtype_.accept(this, arg)

  // Members declared in BoolType.Visitor
  override def visit(x: syntax.BoolType1, arg: A) = ir.BoolType

  // Members declared in MapType.Visitor
  override def visit(x: syntax.MapType1, arg: A) =
    ir.MapType(x.type_1.accept(this, arg), x.type_2.accept(this, arg))

  // Members declared in IntType.Visitor
  override def visit(x: syntax.IntType1, arg: A) = ir.IntType

  // Members declared in BVType.Visitor
  override def visit(x: syntax.BVType1, arg: A) = ir.BitVecType(x.bvtype_.stripPrefix("bv").toInt)
}

private def ensureTypesVisitorHasNoAbstractMembers[A] = new TypesBNFCVisitor[A] {}
