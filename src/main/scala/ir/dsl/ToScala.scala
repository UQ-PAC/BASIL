package ir.dsl

import ir.*
import util.{Twine, StringEscape, indent, indentNested, intersperse}
import translating.{BasilIR, BasilIRExp}

import collection.immutable.{SortedMap}
import collection.immutable.{LazyList}
import collection.mutable
import collection.mutable.{LinkedHashSet}

/**
 * Trait supporting the conversion of objects to stringified Scala source code, suitable
 * for use verbatim in a .scala file. (In some cases, additional imports may be needed.)
 *
 * For example, the following Scala expressions evaluate to true:
 *
 *     "hi".toScala == "\"hi\""
 *     TrueLiteral.toScala == "TrueLiteral"
 *     123.toScala == "123"
 *     BigInt(123).toScala == "BigInt(\"123\")"
 *
 * The main function provided is a .toScala extension method which returns a String.
 *
 * When defining ToScala[A], if the Scala source code will span multiple lines,
 * then `toScalaLines` should be implemented instead. This allows for more efficient
 * manipulation and indentation. See the definition of `Twine` for more details.
 *
 * In such cases, `toScalaLines` should be overriden and `toScalaUsingToScalaLines`
 * should be used to define `toScala`.
 *
 */
trait ToScala[-T]:
  protected def toScalaUsingToScalaLines(x: T): String = x.toScalaLines.mkString

  extension (x: T)
    def toScala: String

    // NOTE: default implementation does not check for newlines within the toScala output!
    // embedded newlines are not permitted by the Twine conventions.
    def toScalaLines: Twine = LazyList(x.toScala)


// generated from ./expr.json
given ToScala[Expr] with
  extension (x: Expr) def toScala: String = x match {
    case x: Literal => x match {
      case x: BoolLit => x match {
        case x: TrueLiteral.type => s"TrueLiteral"
        case x: FalseLiteral.type => s"FalseLiteral"
      }
      case x: BitVecLiteral => s"BitVecLiteral(${x.value.toScala}, ${x.size.toScala})"
      case x: IntLiteral => s"IntLiteral(${x.value.toScala})"
    }
    case x: Extract => s"Extract(${x.end.toScala}, ${x.start.toScala}, ${x.body.toScala})"
    case x: Repeat => s"Repeat(${x.repeats.toScala}, ${x.body.toScala})"
    case x: ZeroExtend => s"ZeroExtend(${x.extension.toScala}, ${x.body.toScala})"
    case x: SignExtend => s"SignExtend(${x.extension.toScala}, ${x.body.toScala})"
    case x: UnaryExpr => s"UnaryExpr(${x.op.toScala}, ${x.arg.toScala})"
    case x: BinaryExpr => s"BinaryExpr(${x.op.toScala}, ${x.arg1.toScala}, ${x.arg2.toScala})"
    case x: UninterpretedFunction => s"UninterpretedFunction(${x.name.toScala}, ${x.params.toScala}, ${x.returnType.toScala})"
    case x: Variable => x match {
      case x: Register => s"Register(${x.name.toScala}, ${x.size.toScala})"
      case x: LocalVar => s"LocalVar(${x.varName.toScala}, ${x.irType.toScala}, ${x.index.toScala})"
    }
  }

given ToScala[UnOp] with
  extension (x: UnOp) override def toScala: String = x match {
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
  extension (x: BinOp) override def toScala: String = x match {
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
  extension (x: Global) override def toScala: String = x match {
    case x: Register => s"Register(${x.name.toScala}, ${x.size.toScala})"
    case x: Memory => x match {
      case x: StackMemory => s"StackMemory(${x.name.toScala}, ${x.addressSize.toScala}, ${x.valueSize.toScala})"
      case x: SharedMemory => s"SharedMemory(${x.name.toScala}, ${x.addressSize.toScala}, ${x.valueSize.toScala})"
    }
  }

// end generated from ./expr.json

// generated from ./statements.json
given ToScala[Command] with
  extension (x: Command) override def toScala: String = x match {
    case x: Statement => x match {
      case x: Assign => x match {
        case x: SingleAssign => x match {
          case x: LocalAssign => s"LocalAssign(${x.lhs.toScala}, ${x.rhs.toScala}, ${x.label.toScala})"
          case x: MemoryLoad => s"MemoryLoad(${x.lhs.toScala}, ${x.mem.toScala}, ${x.index.toScala}, ${x.endian.toScala}, ${x.size.toScala}, ${x.label.toScala})"
        }
        case x: DirectCall => summon[ToScala[DirectCall]].toScala(x)
      }
      case x: MemoryStore => s"MemoryStore(${x.mem.toScala}, ${x.index.toScala}, ${x.value.toScala}, ${x.endian.toScala}, ${x.size.toScala}, ${x.label.toScala})"
      case x: NOP => s"NOP(${x.label.toScala})"
      case x: Assert => s"Assert(${x.body.toScala}, ${x.comment.toScala}, ${x.label.toScala})"
      case x: Assume => s"Assume(${x.body.toScala}, ${x.comment.toScala}, ${x.label.toScala}, ${x.checkSecurity.toScala})"
      case x: Call => x match {
        case x: DirectCall => summon[ToScala[DirectCall]].toScala(x)
        case x: IndirectCall => summon[ToScala[IndirectCall]].toScala(x)
      }
    }
    case x: Jump => x match {
      case x: Unreachable => s"Unreachable(${x.label.toScala})"
      case x: Return => summon[ToScala[Return]].toScala(x)
      case x: GoTo => summon[ToScala[GoTo]].toScala(x)
    }
  }

// end generated from ./statements.json

// generated from ./irtype.json
given ToScala[IRType] with
  extension (x: IRType) override def toScala: String = x match {
    case x: BoolType.type => s"BoolType"
    case x: IntType.type => s"IntType"
    case x: BitVecType => s"BitVecType(${x.size.toScala})"
    case x: MapType => s"MapType(${x.param.toScala}, ${x.result.toScala})"
  }

// end generated from ./irtype.json


given ToScala[Return] with
  extension (x: Return) override def toScala: String = "ret"
given ToScala[DirectCall] with
  extension (x: DirectCall) override def toScala: String = s"directCall(${x.target.procName.toScala})"
given ToScala[IndirectCall] with
  extension (x: IndirectCall) override def toScala: String = s"indirectCall(${x.target.toScala})"
given ToScala[GoTo] with
  extension (x: GoTo) override def toScala: String = s"goto(${x.targets.map(x => x.label.toScala).mkString(", ")})"

given ToScala[Block] with
  extension (x: Block)
    def toScala = toScalaUsingToScalaLines(x)
    override def toScalaLines: Twine =
      val commands = x.statements ++ LazyList(x.jump)
      indentNested(
        s"block(${x.label.toScala}",
        commands.to(LazyList).map(_.toScalaLines),
        ")",
        headSep = true
      )

given ToScala[Procedure] with
  extension (x: Procedure)
    def toScala = toScalaUsingToScalaLines(x)
    override def toScalaLines: Twine =
      indentNested(
        s"proc(${x.procName.toScala}",
        x.blocks.to(LazyList).map(_.toScalaLines),
        ")",
        headSep = true
      )

given ToScala[Program] with
  extension (x: Program)
    def toScala = toScalaUsingToScalaLines(x)
    override def toScalaLines: Twine =
      indentNested(
        "prog(",
        x.procedures.to(LazyList).map(_.toScalaLines),
        ")"
      )

given ToScala[String] with
  extension (x: String) override def toScala: String = StringEscape.quote(x)
given ToScala[Endian] with
  extension (x: Endian) override def toScala: String = "Endian." + x.toString()
given ToScala[Int] with
  extension (x: Int) override def toScala: String = x.toString()
given ToScala[Boolean] with
  extension (x: Boolean) override def toScala: String = x.toString()
given ToScala[BigInt] with
  extension (x: BigInt) override def toScala: String = s"BigInt(${x.toString.toScala})"


given [T](using ToScala[T]): ToScala[Seq[T]] with
  extension (x: Seq[T]) override def toScala: String = x match
    case Seq() => "Seq()"
    case Seq(x) => s"Seq(${x.toScala})"
    case _ => s"Seq(${x.map(_.toScala).mkString(", ")})"

given [T](using ToScala[T]): ToScala[LinkedHashSet[T]] with
  extension (x: LinkedHashSet[T]) override def toScala: String =
    s"LinkedHashSet(${x.map(_.toScala).mkString(", ")})"


given [T](using ToScala[T]): ToScala[Option[T]] with
  extension (x: Option[T]) override def toScala: String = x match
    case None => "None"
    case Some(x) => s"Some(${x.toScala})"

given [K,V](using ToScala[K])(using ToScala[V]): ToScala[SortedMap[K,V]] with
  extension (x: SortedMap[K,V]) override def toScala: String =
    val entries = x.map((a,b) => s"${a.toScala} -> ${b.toScala}").mkString(", ")
    s"SortedMap($entries)"
