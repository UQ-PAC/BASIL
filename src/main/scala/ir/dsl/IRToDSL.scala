package ir.dsl

import ir.*
import translating.{BasilIR, BasilIRExp}

import collection.immutable.{SortedMap}
import collection.mutable
import collection.mutable.{LinkedHashSet}

trait ToScala[-T]:
  def toScala(x: T): String



// generated from ./expr.json
given ToScala[Expr] with
  def toScala(x: Expr): String = x match {
    case x: Literal => x match {
      case x: BoolLit => x match {
        case x: TrueLiteral.type => s"TrueLiteral"
        case x: FalseLiteral.type => s"FalseLiteral"
      }
      case x: BitVecLiteral => s"BitVecLiteral(${summon[ToScala[BigInt]].toScala(x.value)}, ${summon[ToScala[Int]].toScala(x.size)})"
      case x: IntLiteral => s"IntLiteral(${summon[ToScala[BigInt]].toScala(x.value)})"
    }
    case x: Extract => s"Extract(${summon[ToScala[Int]].toScala(x.end)}, ${summon[ToScala[Int]].toScala(x.start)}, ${summon[ToScala[Expr]].toScala(x.body)})"
    case x: Repeat => s"Repeat(${summon[ToScala[Int]].toScala(x.repeats)}, ${summon[ToScala[Expr]].toScala(x.body)})"
    case x: ZeroExtend => s"ZeroExtend(${summon[ToScala[Int]].toScala(x.extension)}, ${summon[ToScala[Expr]].toScala(x.body)})"
    case x: SignExtend => s"SignExtend(${summon[ToScala[Int]].toScala(x.extension)}, ${summon[ToScala[Expr]].toScala(x.body)})"
    case x: UnaryExpr => s"UnaryExpr(${summon[ToScala[UnOp]].toScala(x.op)}, ${summon[ToScala[Expr]].toScala(x.arg)})"
    case x: BinaryExpr => s"BinaryExpr(${summon[ToScala[BinOp]].toScala(x.op)}, ${summon[ToScala[Expr]].toScala(x.arg1)}, ${summon[ToScala[Expr]].toScala(x.arg2)})"
    case x: UninterpretedFunction => s"UninterpretedFunction(${summon[ToScala[String]].toScala(x.name)}, ${summon[ToScala[Seq[Expr]]].toScala(x.params)}, ${summon[ToScala[IRType]].toScala(x.returnType)})"
    case x: Variable => x match {
      case x: Register => s"Register(${summon[ToScala[String]].toScala(x.name)}, ${summon[ToScala[Int]].toScala(x.size)})"
      case x: LocalVar => s"LocalVar(${summon[ToScala[String]].toScala(x.varName)}, ${summon[ToScala[IRType]].toScala(x.irType)}, ${summon[ToScala[Int]].toScala(x.index)})"
    }
  }

given ToScala[UnOp] with
  def toScala(x: UnOp): String = x match {
    case x: BoolUnOp => x match {
      case x: BoolNOT.type => s"BoolNOT"
      case x: BoolToBV1.type => s"BoolToBV1"
    }
    case x: IntUnOp => x match {
      case x: IntNEG.type => s"IntNEG"
    }
    case x: BVUnOp => x match {
      case x: BVNOT.type => s"BVNOT"
      case x: BVNEG.type => s"BVNEG"
    }
  }

given ToScala[BinOp] with
  def toScala(x: BinOp): String = x match {
    case x: BoolBinOp => x match {
      case x: BoolEQ.type => s"BoolEQ"
      case x: BoolNEQ.type => s"BoolNEQ"
      case x: BoolAND.type => s"BoolAND"
      case x: BoolOR.type => s"BoolOR"
      case x: BoolIMPLIES.type => s"BoolIMPLIES"
      case x: BoolEQUIV.type => s"BoolEQUIV"
    }
    case x: BVBinOp => x match {
      case x: BVAND.type => s"BVAND"
      case x: BVOR.type => s"BVOR"
      case x: BVADD.type => s"BVADD"
      case x: BVMUL.type => s"BVMUL"
      case x: BVUDIV.type => s"BVUDIV"
      case x: BVUREM.type => s"BVUREM"
      case x: BVSHL.type => s"BVSHL"
      case x: BVLSHR.type => s"BVLSHR"
      case x: BVULT.type => s"BVULT"
      case x: BVNAND.type => s"BVNAND"
      case x: BVNOR.type => s"BVNOR"
      case x: BVXOR.type => s"BVXOR"
      case x: BVXNOR.type => s"BVXNOR"
      case x: BVCOMP.type => s"BVCOMP"
      case x: BVSUB.type => s"BVSUB"
      case x: BVSDIV.type => s"BVSDIV"
      case x: BVSREM.type => s"BVSREM"
      case x: BVSMOD.type => s"BVSMOD"
      case x: BVASHR.type => s"BVASHR"
      case x: BVULE.type => s"BVULE"
      case x: BVUGT.type => s"BVUGT"
      case x: BVUGE.type => s"BVUGE"
      case x: BVSLT.type => s"BVSLT"
      case x: BVSLE.type => s"BVSLE"
      case x: BVSGT.type => s"BVSGT"
      case x: BVSGE.type => s"BVSGE"
      case x: BVEQ.type => s"BVEQ"
      case x: BVNEQ.type => s"BVNEQ"
      case x: BVCONCAT.type => s"BVCONCAT"
    }
    case x: IntBinOp => x match {
      case x: IntADD.type => s"IntADD"
      case x: IntMUL.type => s"IntMUL"
      case x: IntSUB.type => s"IntSUB"
      case x: IntDIV.type => s"IntDIV"
      case x: IntMOD.type => s"IntMOD"
      case x: IntEQ.type => s"IntEQ"
      case x: IntNEQ.type => s"IntNEQ"
      case x: IntLT.type => s"IntLT"
      case x: IntLE.type => s"IntLE"
      case x: IntGT.type => s"IntGT"
      case x: IntGE.type => s"IntGE"
    }
  }

given ToScala[Global] with
  def toScala(x: Global): String = x match {
    case x: Register => s"Register(${summon[ToScala[String]].toScala(x.name)}, ${summon[ToScala[Int]].toScala(x.size)})"
    case x: Memory => x match {
      case x: StackMemory => s"StackMemory(${summon[ToScala[String]].toScala(x.name)}, ${summon[ToScala[Int]].toScala(x.addressSize)}, ${summon[ToScala[Int]].toScala(x.valueSize)})"
      case x: SharedMemory => s"SharedMemory(${summon[ToScala[String]].toScala(x.name)}, ${summon[ToScala[Int]].toScala(x.addressSize)}, ${summon[ToScala[Int]].toScala(x.valueSize)})"
    }
  }

// end generated

// generated from ./statements.json
given ToScala[Command] with
  def toScala(x: Command): String = x match {
    case x: Statement => x match {
      case x: Assign => x match {
        case x: SingleAssign => x match {
          case x: LocalAssign => s"LocalAssign(${summon[ToScala[Variable]].toScala(x.lhs)}, ${summon[ToScala[Expr]].toScala(x.rhs)}, ${summon[ToScala[Option[String]]].toScala(x.label)})"
          case x: MemoryLoad => s"MemoryLoad(${summon[ToScala[Variable]].toScala(x.lhs)}, ${summon[ToScala[Memory]].toScala(x.mem)}, ${summon[ToScala[Expr]].toScala(x.index)}, ${summon[ToScala[Endian]].toScala(x.endian)}, ${summon[ToScala[Int]].toScala(x.size)}, ${summon[ToScala[Option[String]]].toScala(x.label)})"
        }
        case x: DirectCall => summon[ToScala[DirectCall]].toScala(x)
      }
      case x: MemoryStore => s"MemoryStore(${summon[ToScala[Memory]].toScala(x.mem)}, ${summon[ToScala[Expr]].toScala(x.index)}, ${summon[ToScala[Expr]].toScala(x.value)}, ${summon[ToScala[Endian]].toScala(x.endian)}, ${summon[ToScala[Int]].toScala(x.size)}, ${summon[ToScala[Option[String]]].toScala(x.label)})"
      case x: NOP => s"NOP(${summon[ToScala[Option[String]]].toScala(x.label)})"
      case x: Assert => s"Assert(${summon[ToScala[Expr]].toScala(x.body)}, ${summon[ToScala[Option[String]]].toScala(x.comment)}, ${summon[ToScala[Option[String]]].toScala(x.label)})"
      case x: Assume => s"Assume(${summon[ToScala[Expr]].toScala(x.body)}, ${summon[ToScala[Option[String]]].toScala(x.comment)}, ${summon[ToScala[Option[String]]].toScala(x.label)}, ${summon[ToScala[Boolean]].toScala(x.checkSecurity)})"
      case x: Call => x match {
        case x: DirectCall => summon[ToScala[DirectCall]].toScala(x)
        case x: IndirectCall => summon[ToScala[IndirectCall]].toScala(x)
      }
    }
    case x: Jump => x match {
      case x: Unreachable => s"Unreachable(${summon[ToScala[Option[String]]].toScala(x.label)})"
      case x: Return => summon[ToScala[Return]].toScala(x)
      case x: GoTo => summon[ToScala[GoTo]].toScala(x)
    }
  }

// end generated

// generated from irtype.json
given ToScala[IRType] with
  def toScala(x: IRType): String = x match {
    case x: BoolType.type => s"BoolType"
    case x: IntType.type => s"IntType"
    case x: BitVecType => s"BitVecType(${summon[ToScala[Int]].toScala(x.size)})"
    case x: MapType => s"MapType(${summon[ToScala[IRType]].toScala(x.param)}, ${summon[ToScala[IRType]].toScala(x.result)})"
  }

// end generated


given ToScala[Return] with
  def toScala(x: Return): String = "ret"
given ToScala[DirectCall] with
  def toScala(x: DirectCall): String =
    val str = summon[ToScala[String]]
    s"directCall(${str.toScala(x.target.procName)}\")"
given ToScala[IndirectCall] with
  def toScala(x: IndirectCall): String =
    val v = summon[ToScala[Variable]]
    val str = summon[ToScala[String]]
    s"indirectCall(${v.toScala(x.target)})"
given ToScala[GoTo] with
  val str = summon[ToScala[String]]
  def toScala(x: GoTo): String = s"goto(${x.targets.map(x => str.toScala(x.label)).mkString(", ")})"

given ToScala[Block] with
  def toScala(x: Block): String =
    val s = summon[ToScala[Statement]]
    val str = summon[ToScala[String]]
    val commands = x.statements ++ Seq(x.jump)
    s"block(${str.toScala(x.label)},\n      " + (commands.map(s.toScala).mkString(",\n      ")) + "\n    )"

given ToScala[Procedure] with
  def toScala(x: Procedure): String =
    val b = summon[ToScala[Block]]
    val str = summon[ToScala[String]]
    s"proc(${str.toScala(x.procName)},\n    " + (x.blocks.map(b.toScala).mkString(",\n    ")) + "\n  )"

given ToScala[Program] with
  def toScala(x: Program): String =
    val p = summon[ToScala[Procedure]]
    val str = summon[ToScala[String]]
    s"prog(\n  " + (x.procedures.map(p.toScala).mkString(",\n  ")) + "\n)"

given ToScala[String] with
  def toScala(x: String): String = StringEscape.quote(x)
given ToScala[Endian] with
  def toScala(x: Endian): String = "Endian." + x.toString()
given ToScala[Int] with
  def toScala(x: Int): String = x.toString()
given ToScala[Boolean] with
  def toScala(x: Boolean): String = x.toString()
given ToScala[BigInt] with
  def toScala(x: BigInt): String = s"BigInt(\"$x\")"


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

extension (x: Statement)
  def toScala: String = summon[ToScala[Statement]].toScala(x)
extension (x: Procedure)
  def toScala: String = summon[ToScala[Procedure]].toScala(x)
extension (x: Program)
  def toScala: String = summon[ToScala[Program]].toScala(x)
