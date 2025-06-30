package ir.parsing

/**
 * Data used to construct either a [[ir.LocalVar]] or [[ir.Register]].
 * The boolean value is true if local, or false if global (register).
 */
type LVarSpec = (String, ir.IRType, Boolean)

type BaseParseTypes = ir.dsl.EventuallyProgram | ir.dsl.EventuallyProcedure | ir.dsl.EventuallyBlock |
  ir.dsl.DSLStatement | ir.Expr | ir.BinOp | ir.UnOp | ir.IRType | ir.Endian | ir.Variable | ir.Memory | String |
  BigInt | LVarSpec

type ParseTypes = BaseParseTypes | List[BaseParseTypes] | Option[BaseParseTypes] | Map[String, BaseParseTypes]

/**
 * A value parsed out of the BNFC [[basil_ir.Absyn]] AST. This can be one of a
 * number of Basil IR types. The user of the [[ir.parsing.BasilParseValue]] should
 * use one of the member methods (e.g., [[ir.parsing.BasilParseValue#expr]]) to cast
 * the value to the required type.
 *
 * This implements a kind of *dynamic typing*. This is required so we can implement the
 * [[basil_ir.AllVisitor]] interface which requires that all AST structures return a
 * value of a single common type.
 */
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
  def lvar = x.asInstanceOf[LVarSpec]
  def endian = x.asInstanceOf[ir.Endian]
  def list[T](f: BasilParseValue => T) =
    x.asInstanceOf[List[BaseParseTypes]].map(x => f(BasilParseValue(x)))
  def opt[T](f: BasilParseValue => T) =
    x.asInstanceOf[Option[BaseParseTypes]].map(x => f(BasilParseValue(x)))
  def map[T](f: BasilParseValue => T) =
    x.asInstanceOf[Map[String, BaseParseTypes]].view.mapValues(x => f(BasilParseValue(x))).toMap
  def stmt = x.asInstanceOf[ir.dsl.DSLStatement]
  def block = x.asInstanceOf[ir.dsl.EventuallyBlock]
  def proc = x.asInstanceOf[ir.dsl.EventuallyProcedure]
  def prog = x.asInstanceOf[ir.dsl.EventuallyProgram]
  def memory = x.asInstanceOf[ir.Memory]
  def register = x.asInstanceOf[ir.GlobalVar]
}

private object BasilParseValue {
  given Conversion[ParseTypes, BasilParseValue] with
    def apply(x: ParseTypes) = BasilParseValue(x)
}
