package ir.parsing

private type DSLStatement = ir.dsl.NonCallStatement | ir.dsl.EventuallyStatement | ir.dsl.EventuallyJump

private case object UndefinedParseResult

private type BaseParseTypes = ir.dsl.EventuallyProgram | ir.dsl.EventuallyProcedure | ir.dsl.EventuallyBlock |
  DSLStatement | ir.Expr | ir.BinOp | ir.UnOp | ir.IRType | ir.Endian | ir.Variable | ir.Memory | String | BigInt |
  UndefinedParseResult.type

private type ParseTypes = BaseParseTypes | List[BaseParseTypes] | Option[BaseParseTypes] | Map[String, BaseParseTypes]

case class BasilParseValue(x: ParseTypes) {
  def ty = x.asInstanceOf[ir.IRType]
  def bvty = x.asInstanceOf[ir.BitVecType]
  def binop = x.asInstanceOf[ir.BinOp]
  def unop = x.asInstanceOf[ir.UnOp]
  def expr = x.asInstanceOf[ir.Expr]
  def str = x.asInstanceOf[String]
  def int = x.asInstanceOf[BigInt]
  def int32 =
    val y = int
    assert(y.isValidInt, "BigInt value is out of range for a 32-bit integer")
    y.toInt
  def v = x.asInstanceOf[ir.Variable]
  def endian = x.asInstanceOf[ir.Endian]
  def list[T](f: BasilParseValue => T) =
    x.asInstanceOf[List[BaseParseTypes]].map(x => f(BasilParseValue(x)))
  def opt[T](f: BasilParseValue => T) =
    x.asInstanceOf[Option[BaseParseTypes]].map(x => f(BasilParseValue(x)))
  def map[T](f: BasilParseValue => T) =
    x.asInstanceOf[Map[String, BaseParseTypes]].view.mapValues(x => f(BasilParseValue(x))).toMap
  def stmt = x.asInstanceOf[DSLStatement]
  def block = x.asInstanceOf[ir.dsl.EventuallyBlock]
  def proc = x.asInstanceOf[ir.dsl.EventuallyProcedure]
  def prog = x.asInstanceOf[ir.dsl.EventuallyProgram]
  def memory = x.asInstanceOf[ir.Memory]
  def register = x.asInstanceOf[ir.Register]
}

object BasilParseValue {
  given Conversion[ParseTypes, BasilParseValue] with
    def apply(x: ParseTypes) = BasilParseValue(x)
}
