package ir.parsing

import basil_ir.Absyn as syntax

import scala.collection.immutable.ListMap
import scala.jdk.CollectionConverters.*

trait AttributeListBNFCVisitor[A]
    extends syntax.AttribSet.Visitor[Attrib.Map, A],
      syntax.Attr.Visitor[Attrib, A],
      syntax.AttrKeyValue.Visitor[(String, Attrib), A],
      LiteralsBNFCVisitor[A] {

  override def visit(x: syntax.Attr_Map, arg: A): ir.parsing.Attrib =
    Attrib.Map(ListMap.from(x.listattrkeyvalue_.asScala.toList.map(_.accept(this, arg))))
  override def visit(x: syntax.Attr_List, arg: A): ir.parsing.Attrib =
    Attrib.List(x.listattr_.asScala.toVector.map(_.accept(this, arg)))
  override def visit(x: syntax.Attr_Lit, arg: A): ir.parsing.Attrib =
    Attrib.ValLiteral(x.value_.accept(this, arg))
  override def visit(x: syntax.Attr_Str, arg: A): ir.parsing.Attrib = {
    Attrib.ValString(unquote(x.str_, x))
  }

  override def visit(p: syntax.AttrKeyValue1, arg: A): (String, Attrib) =
    (unsigilAttrib(p.bident_), p.attr_.accept(this, arg))

  override def visit(p: syntax.AttribSet_Empty, arg: A): Attrib.Map = Attrib.Map(ListMap())
  override def visit(p: syntax.AttribSet_Some, arg: A): Attrib.Map = {
    Attrib.Map(ListMap.from(p.listattrkeyvalue_.asScala.map(_.accept(this, arg))))
  }
}
