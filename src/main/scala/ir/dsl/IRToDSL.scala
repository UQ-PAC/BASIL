package ir.dsl

import ir.*
import translating.{BasilIR, BasilIRExp}

import collection.immutable.{SortedMap}
import collection.mutable
import collection.mutable.{LinkedHashSet}

trait ToScala[T]:
  def toScala(x: T): String

// generated from ./expr.json
given ToScala[TrueLiteral.type] with
  def toScala(x: TrueLiteral.type) = s"TrueLiteral"
given ToScala[FalseLiteral.type] with
  def toScala(x: FalseLiteral.type) = s"FalseLiteral"
given ToScala[BitVecLiteral] with
  def toScala(x: BitVecLiteral) = s"BitVecLiteral(${summon[ToScala[BigInt]].toScala(x.value)}, ${summon[ToScala[Int]].toScala(x.size)})"
given ToScala[IntLiteral] with
  def toScala(x: IntLiteral) = s"IntLiteral(${summon[ToScala[BigInt]].toScala(x.value)})"
given ToScala[Extract] with
  def toScala(x: Extract) = s"Extract(${summon[ToScala[Int]].toScala(x.end)}, ${summon[ToScala[Int]].toScala(x.start)}, ${summon[ToScala[Expr]].toScala(x.body)})"
given ToScala[Repeat] with
  def toScala(x: Repeat) = s"Repeat(${summon[ToScala[Int]].toScala(x.repeats)}, ${summon[ToScala[Expr]].toScala(x.body)})"
given ToScala[ZeroExtend] with
  def toScala(x: ZeroExtend) = s"ZeroExtend(${summon[ToScala[Int]].toScala(x.extension)}, ${summon[ToScala[Expr]].toScala(x.body)})"
given ToScala[SignExtend] with
  def toScala(x: SignExtend) = s"SignExtend(${summon[ToScala[Int]].toScala(x.extension)}, ${summon[ToScala[Expr]].toScala(x.body)})"
given ToScala[UnaryExpr] with
  def toScala(x: UnaryExpr) = s"UnaryExpr(${summon[ToScala[UnOp]].toScala(x.op)}, ${summon[ToScala[Expr]].toScala(x.arg)})"
given ToScala[BoolNOT.type] with
  def toScala(x: BoolNOT.type) = s"BoolNOT"
given ToScala[BoolToBV1.type] with
  def toScala(x: BoolToBV1.type) = s"BoolToBV1"
given ToScala[IntNEG.type] with
  def toScala(x: IntNEG.type) = s"IntNEG"
given ToScala[BVNOT.type] with
  def toScala(x: BVNOT.type) = s"BVNOT"
given ToScala[BVNEG.type] with
  def toScala(x: BVNEG.type) = s"BVNEG"
given ToScala[BinaryExpr] with
  def toScala(x: BinaryExpr) = s"BinaryExpr(${summon[ToScala[BinOp]].toScala(x.op)}, ${summon[ToScala[Expr]].toScala(x.arg1)}, ${summon[ToScala[Expr]].toScala(x.arg2)})"
given ToScala[BoolEQ.type] with
  def toScala(x: BoolEQ.type) = s"BoolEQ"
given ToScala[BoolNEQ.type] with
  def toScala(x: BoolNEQ.type) = s"BoolNEQ"
given ToScala[BoolAND.type] with
  def toScala(x: BoolAND.type) = s"BoolAND"
given ToScala[BoolOR.type] with
  def toScala(x: BoolOR.type) = s"BoolOR"
given ToScala[BoolIMPLIES.type] with
  def toScala(x: BoolIMPLIES.type) = s"BoolIMPLIES"
given ToScala[BoolEQUIV.type] with
  def toScala(x: BoolEQUIV.type) = s"BoolEQUIV"
given ToScala[BVAND.type] with
  def toScala(x: BVAND.type) = s"BVAND"
given ToScala[BVOR.type] with
  def toScala(x: BVOR.type) = s"BVOR"
given ToScala[BVADD.type] with
  def toScala(x: BVADD.type) = s"BVADD"
given ToScala[BVMUL.type] with
  def toScala(x: BVMUL.type) = s"BVMUL"
given ToScala[BVUDIV.type] with
  def toScala(x: BVUDIV.type) = s"BVUDIV"
given ToScala[BVUREM.type] with
  def toScala(x: BVUREM.type) = s"BVUREM"
given ToScala[BVSHL.type] with
  def toScala(x: BVSHL.type) = s"BVSHL"
given ToScala[BVLSHR.type] with
  def toScala(x: BVLSHR.type) = s"BVLSHR"
given ToScala[BVULT.type] with
  def toScala(x: BVULT.type) = s"BVULT"
given ToScala[BVNAND.type] with
  def toScala(x: BVNAND.type) = s"BVNAND"
given ToScala[BVNOR.type] with
  def toScala(x: BVNOR.type) = s"BVNOR"
given ToScala[BVXOR.type] with
  def toScala(x: BVXOR.type) = s"BVXOR"
given ToScala[BVXNOR.type] with
  def toScala(x: BVXNOR.type) = s"BVXNOR"
given ToScala[BVCOMP.type] with
  def toScala(x: BVCOMP.type) = s"BVCOMP"
given ToScala[BVSUB.type] with
  def toScala(x: BVSUB.type) = s"BVSUB"
given ToScala[BVSDIV.type] with
  def toScala(x: BVSDIV.type) = s"BVSDIV"
given ToScala[BVSREM.type] with
  def toScala(x: BVSREM.type) = s"BVSREM"
given ToScala[BVSMOD.type] with
  def toScala(x: BVSMOD.type) = s"BVSMOD"
given ToScala[BVASHR.type] with
  def toScala(x: BVASHR.type) = s"BVASHR"
given ToScala[BVULE.type] with
  def toScala(x: BVULE.type) = s"BVULE"
given ToScala[BVUGT.type] with
  def toScala(x: BVUGT.type) = s"BVUGT"
given ToScala[BVUGE.type] with
  def toScala(x: BVUGE.type) = s"BVUGE"
given ToScala[BVSLT.type] with
  def toScala(x: BVSLT.type) = s"BVSLT"
given ToScala[BVSLE.type] with
  def toScala(x: BVSLE.type) = s"BVSLE"
given ToScala[BVSGT.type] with
  def toScala(x: BVSGT.type) = s"BVSGT"
given ToScala[BVSGE.type] with
  def toScala(x: BVSGE.type) = s"BVSGE"
given ToScala[BVEQ.type] with
  def toScala(x: BVEQ.type) = s"BVEQ"
given ToScala[BVNEQ.type] with
  def toScala(x: BVNEQ.type) = s"BVNEQ"
given ToScala[BVCONCAT.type] with
  def toScala(x: BVCONCAT.type) = s"BVCONCAT"
given ToScala[IntADD.type] with
  def toScala(x: IntADD.type) = s"IntADD"
given ToScala[IntMUL.type] with
  def toScala(x: IntMUL.type) = s"IntMUL"
given ToScala[IntSUB.type] with
  def toScala(x: IntSUB.type) = s"IntSUB"
given ToScala[IntDIV.type] with
  def toScala(x: IntDIV.type) = s"IntDIV"
given ToScala[IntMOD.type] with
  def toScala(x: IntMOD.type) = s"IntMOD"
given ToScala[IntEQ.type] with
  def toScala(x: IntEQ.type) = s"IntEQ"
given ToScala[IntNEQ.type] with
  def toScala(x: IntNEQ.type) = s"IntNEQ"
given ToScala[IntLT.type] with
  def toScala(x: IntLT.type) = s"IntLT"
given ToScala[IntLE.type] with
  def toScala(x: IntLE.type) = s"IntLE"
given ToScala[IntGT.type] with
  def toScala(x: IntGT.type) = s"IntGT"
given ToScala[IntGE.type] with
  def toScala(x: IntGE.type) = s"IntGE"
given ToScala[UninterpretedFunction] with
  def toScala(x: UninterpretedFunction) = s"UninterpretedFunction(${summon[ToScala[String]].toScala(x.name)}, ${summon[ToScala[Seq[Expr]]].toScala(x.params)}, ${summon[ToScala[IRType]].toScala(x.returnType)})"
given ToScala[Register] with
  def toScala(x: Register) = s"Register(${summon[ToScala[String]].toScala(x.name)}, ${summon[ToScala[Int]].toScala(x.size)})"
given ToScala[LocalVar] with
  def toScala(x: LocalVar) = s"LocalVar(${summon[ToScala[String]].toScala(x.varName)}, ${summon[ToScala[IRType]].toScala(x.irType)}, ${summon[ToScala[Int]].toScala(x.index)})"
given ToScala[StackMemory] with
  def toScala(x: StackMemory) = s"StackMemory(${summon[ToScala[String]].toScala(x.name)}, ${summon[ToScala[Int]].toScala(x.addressSize)}, ${summon[ToScala[Int]].toScala(x.valueSize)})"
given ToScala[SharedMemory] with
  def toScala(x: SharedMemory) = s"SharedMemory(${summon[ToScala[String]].toScala(x.name)}, ${summon[ToScala[Int]].toScala(x.addressSize)}, ${summon[ToScala[Int]].toScala(x.valueSize)})"

// generated from statements.json
given ToScala[LocalAssign] with
  def toScala(x: LocalAssign) = s"LocalAssign(${summon[ToScala[Variable]].toScala(x.lhs)}, ${summon[ToScala[Expr]].toScala(x.rhs)}, ${summon[ToScala[Option[String]]].toScala(x.label)})"
given ToScala[MemoryStore] with
  def toScala(x: MemoryStore) = s"MemoryStore(${summon[ToScala[Memory]].toScala(x.mem)}, ${summon[ToScala[Expr]].toScala(x.index)}, ${summon[ToScala[Expr]].toScala(x.value)}, ${summon[ToScala[Endian]].toScala(x.endian)}, ${summon[ToScala[Int]].toScala(x.size)}, ${summon[ToScala[Option[String]]].toScala(x.label)})"
given ToScala[MemoryLoad] with
  def toScala(x: MemoryLoad) = s"MemoryLoad(${summon[ToScala[Variable]].toScala(x.lhs)}, ${summon[ToScala[Memory]].toScala(x.mem)}, ${summon[ToScala[Expr]].toScala(x.index)}, ${summon[ToScala[Endian]].toScala(x.endian)}, ${summon[ToScala[Int]].toScala(x.size)}, ${summon[ToScala[Option[String]]].toScala(x.label)})"
given ToScala[NOP] with
  def toScala(x: NOP) = s"NOP(${summon[ToScala[Option[String]]].toScala(x.label)})"
given ToScala[Assert] with
  def toScala(x: Assert) = s"Assert(${summon[ToScala[Expr]].toScala(x.body)}, ${summon[ToScala[Option[String]]].toScala(x.comment)}, ${summon[ToScala[Option[String]]].toScala(x.label)})"
given ToScala[Assume] with
  def toScala(x: Assume) = s"Assume(${summon[ToScala[Expr]].toScala(x.body)}, ${summon[ToScala[Option[String]]].toScala(x.comment)}, ${summon[ToScala[Option[String]]].toScala(x.label)}, ${summon[ToScala[Boolean]].toScala(x.checkSecurity)})"
given ToScala[Unreachable] with
  def toScala(x: Unreachable) = s"Unreachable(${summon[ToScala[Option[String]]].toScala(x.label)})"
given ToScala[Return] with
  def toScala(x: Return) = s"Return(${summon[ToScala[Option[String]]].toScala(x.label)}, ${summon[ToScala[SortedMap[LocalVar, Expr]]].toScala(x.outParams)})"
given ToScala[GoTo] with
  def toScala(x: GoTo) = s"GoTo(${summon[ToScala[mutable.LinkedHashSet[Block]]].toScala(x._targets)}, ${summon[ToScala[Option[String]]].toScala(x.label)})"
given ToScala[DirectCall] with
  def toScala(x: DirectCall) = s"DirectCall(${summon[ToScala[Procedure]].toScala(x.target)}, ${summon[ToScala[Option[String]]].toScala(x.label)}, ${summon[ToScala[SortedMap[LocalVar, Variable]]].toScala(x.outParams)}, ${summon[ToScala[SortedMap[LocalVar, Expr]]].toScala(x.actualParams)})"
given ToScala[IndirectCall] with
  def toScala(x: IndirectCall) = s"IndirectCall(${summon[ToScala[Variable]].toScala(x.target)}, ${summon[ToScala[Option[String]]].toScala(x.label)})"


given ToScala[String] with
  def toScala(x: String): String = StringEscape.quote(x)
given ToScala[Endian] with
  def toScala(x: Endian): String = x.toString()
given ToScala[Int] with
  def toScala(x: Int): String = x.toString()
given ToScala[Boolean] with
  def toScala(x: Boolean): String = x.toString()
given ToScala[BigInt] with
  def toScala(x: BigInt): String = s"BigInt($x)"
given ToScala[Expr] with
  def toScala(x: Expr): String = x match
    case _: (Register | TrueLiteral.type | FalseLiteral.type | BitVecLiteral | IntLiteral | Extract | Repeat | ZeroExtend | SignExtend | UnaryExpr | BinaryExpr | LocalVar | UninterpretedFunction)
    => toScala(x)
given ToScala[Memory] with
  def toScala(x: Memory): String = x match
    case _: (StackMemory | SharedMemory) => toScala(x)

given ToScala[Variable] with
  def toScala(x: Variable): String = x match
    case _: (Register | LocalVar) => toScala(x)

given ToScala[IRType] with
  def toScala(x: IRType): String = ???
given ToScala[BinOp] with
  def toScala(x: BinOp): String = ???
given ToScala[UnOp] with
  def toScala(x: UnOp): String = ???

given [T](using t: ToScala[T]): ToScala[Seq[T]] with
  def toScala(x: Seq[T]): String = x match
    case Seq() => "Seq()"
    case Seq(x) => s"Seq(${t.toScala(x)})"
    case _ => s"Seq(${x.map(t.toScala).mkString(", ")})"

given [T](using t: ToScala[T]): ToScala[LinkedHashSet[T]] with
  def toScala(x: LinkedHashSet[T]): String = x match
    case Seq() => "LinkedHashSet()"
    case _ => s"LinkedHashSet(${x.map(t.toScala).mkString(", ")})"


given [T](using t: ToScala[T]): ToScala[Option[T]] with
  def toScala(x: Option[T]): String = x match
    case None => "None"
    case Some(x) => s"Some(${t.toScala(x)})"

given [K,V](using k: ToScala[K])(using v: ToScala[V]): ToScala[SortedMap[K,V]] with
  def toScala(x: SortedMap[K,V]): String =
    val entries = x.map((a,b) => s"${k.toScala(a)} -> ${v.toScala(b)}").mkString(", ")
    s"SortedMap($entries)"

object StringEscape {

  def quote (s: String): String = "\"" + escape(s) + "\""
  def escape(s: String): String = s.flatMap(escapedChar)

  def escapedChar(ch: Char): String = ch match {
    case '\b' => "\\b"
    case '\t' => "\\t"
    case '\n' => "\\n"
    case '\f' => "\\f"
    case '\r' => "\\r"
    case '"'  => "\\\""
    case '\'' => "\\\'"
    case '\\' => "\\\\"
    case _    => if (ch.isControl) "\\0" + Integer.toOctalString(ch.toInt)
                 else              String.valueOf(ch)
  }

}

object Exporter {
  def statementToDSL(s: Statement): DSLStatement = s match
    case s: SingleAssign => s
    case s: MemoryStore  => s
    // case s: MemoryLoad => s // subtype of Assign
    case s: NOP                    => s
    case s: Assert                 => s
    case s: Assume                 => s
    case IndirectCall(target, _)   => indirectCall(target)
    case DirectCall(proc, _, _, _) => directCall(proc.name)
}
