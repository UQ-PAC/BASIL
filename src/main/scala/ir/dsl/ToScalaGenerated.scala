package ir.dsl

import ir.*

/**
 * The end of this file contains generated code to implement ToScala for the various type
 * hierarchies defined by BASIL.
 */

/**
 * XXX: ATTENTION: auto-generated code!
 * ====================================
 *
 * If you are here to manually fix a compilation error, please make sure that
 * you understand the context and carefully apply a specific fix. Make sure
 * that the code maintains the general contract of the ToScala trait.
 *
 * That is, the returned string must be valid Scala code to construct the given
 * object. The `ensure_constructible` functions are defined to match the produced
 * string. Successful compilation of the ensure_constructible functions ensures
 * that the produced strings are valid Scala code.
 *
 * When making manual changes, be sure to change the string literal and the
 * ensure_constrictible functions in the same way.
 */


/**
 * Running the auto-generator
 * --------------------------
 *
 * For large changes, it may be more convenient to re-run the auto-generator
 * instead of manually changing the code.
 *
 * 1. Look below the scissors line (containing "- >8 -") to find the "command:" line.
 *    Take note of which JSON files are mentioned.
 * 2. For each JSON file, find its corresponding Scala file (e.g. statements.json comes
 *    from Statement.scala).
 * 3. Go to https://astexplorer.net/#/gist/eb0f2062180067b412017010df04eace/latest and
 *    make sure that the scalameta setting is set to "Scala 3".
 * 4. Paste the contents of the Scala file into the left side.
 * 5. Copy the JSON from the right-hand side into a new JSON file.
 * 6. Repeat this for the rest of the required JSON files.
 *
 * 7. Run the Python command listed below the scissors line.
 * 8. Make sure that the code compiles and passes tests:
 *
 *     ./mill test.testOnly 'ir.ToScalaTest'
 *
 * 9. Upload the changes.
 *
 */


/**
 * Manually-defined ToScala instances
 * ----------------------------------
 *
 * Externals: DirectCall, IndirectCall, GoTo, Return
 *
 * The "Externals" line above tells the generator the following types as having
 * manually-defined ToScala instances. These types interact with control-flow,
 * and the naive auto-generated code would be large and possibly cyclic.
 */


// NOTE: It is important that these handwritten given instances live in the same
// file as the generated code. These instances must be locatable by summon[],
// otherwise the generated code will self-recurse, leading to non-termination.

given ToScala[Return] with
  extension (x: Return) override def toScala: String = "ret"

given ToScala[DirectCall] with
  extension (x: DirectCall) override def toScala: String = s"directCall(${x.target.procName.toScala})"

given ToScala[IndirectCall] with
  extension (x: IndirectCall) override def toScala: String = s"indirectCall(${x.target.toScala})"

given ToScala[GoTo] with
  extension (x: GoTo) override def toScala: String = s"goto(${x.targets.map(x => x.label.toScala).mkString(", ")})"



// WARNING: Everything below the next line will be overwritten by the generator!
// ------------------------ >8 ------------------------

// format: off

// command:
// scripts/make_repr_functions.py src/main/scala/ir/dsl/ToScalaGenerated.scala ./expr.json ./statements.json ./irtype.json

// generated from ./expr.json
given ToScala[Expr] with
  extension (x: Expr) def toScala: String = x match {
    case x: Literal => x match {
      case x: BoolLit => x match {
        case x: TrueLiteral.type => {
          def ensure_constructible(): TrueLiteral.type = TrueLiteral
          s"TrueLiteral"
        }
        case x: FalseLiteral.type => {
          def ensure_constructible(): FalseLiteral.type = FalseLiteral
          s"FalseLiteral"
        }
      }
      case x: BitVecLiteral => {
        def ensure_constructible(): BitVecLiteral = BitVecLiteral(x.value, x.size)
        s"BitVecLiteral(${x.value.toScala}, ${x.size.toScala})"
      }
      case x: IntLiteral => {
        def ensure_constructible(): IntLiteral = IntLiteral(x.value)
        s"IntLiteral(${x.value.toScala})"
      }
    }
    case x: Extract => {
      def ensure_constructible(): Extract = Extract(x.end, x.start, x.body)
      s"Extract(${x.end.toScala}, ${x.start.toScala}, ${x.body.toScala})"
    }
    case x: Repeat => {
      def ensure_constructible(): Repeat = Repeat(x.repeats, x.body)
      s"Repeat(${x.repeats.toScala}, ${x.body.toScala})"
    }
    case x: ZeroExtend => {
      def ensure_constructible(): ZeroExtend = ZeroExtend(x.extension, x.body)
      s"ZeroExtend(${x.extension.toScala}, ${x.body.toScala})"
    }
    case x: SignExtend => {
      def ensure_constructible(): SignExtend = SignExtend(x.extension, x.body)
      s"SignExtend(${x.extension.toScala}, ${x.body.toScala})"
    }
    case x: UnaryExpr => {
      def ensure_constructible(): UnaryExpr = UnaryExpr(x.op, x.arg)
      s"UnaryExpr(${x.op.toScala}, ${x.arg.toScala})"
    }
    case x: BinaryExpr => {
      def ensure_constructible(): BinaryExpr = BinaryExpr(x.op, x.arg1, x.arg2)
      s"BinaryExpr(${x.op.toScala}, ${x.arg1.toScala}, ${x.arg2.toScala})"
    }
    case x: UninterpretedFunction => {
      def ensure_constructible(): UninterpretedFunction = UninterpretedFunction(x.name, x.params, x.returnType)
      s"UninterpretedFunction(${x.name.toScala}, ${x.params.toScala}, ${x.returnType.toScala})"
    }
    case x: Variable => x match {
      case x: Register => {
        def ensure_constructible(): Register = Register(x.name, x.size)
        s"Register(${x.name.toScala}, ${x.size.toScala})"
      }
      case x: LocalVar => {
        def ensure_constructible(): LocalVar = LocalVar(x.varName, x.irType, x.index)
        s"LocalVar(${x.varName.toScala}, ${x.irType.toScala}, ${x.index.toScala})"
      }
    }
  }

given ToScala[UnOp] with
  extension (x: UnOp) def toScala: String = x match {
    case x: BoolUnOp => x match {
      case x: BoolNOT.type => {
        def ensure_constructible(): BoolNOT.type = BoolNOT
        s"BoolNOT"
      }
      case x: BoolToBV1.type => {
        def ensure_constructible(): BoolToBV1.type = BoolToBV1
        s"BoolToBV1"
      }
    }
    case x: IntUnOp => x match {
      case x: IntNEG.type => {
        def ensure_constructible(): IntNEG.type = IntNEG
        s"IntNEG"
      }
    }
    case x: BVUnOp => x match {
      case x: BVNOT.type => {
        def ensure_constructible(): BVNOT.type = BVNOT
        s"BVNOT"
      }
      case x: BVNEG.type => {
        def ensure_constructible(): BVNEG.type = BVNEG
        s"BVNEG"
      }
    }
  }

given ToScala[BinOp] with
  extension (x: BinOp) def toScala: String = x match {
    case x: BoolBinOp => x match {
      case x: BoolEQ.type => {
        def ensure_constructible(): BoolEQ.type = BoolEQ
        s"BoolEQ"
      }
      case x: BoolNEQ.type => {
        def ensure_constructible(): BoolNEQ.type = BoolNEQ
        s"BoolNEQ"
      }
      case x: BoolAND.type => {
        def ensure_constructible(): BoolAND.type = BoolAND
        s"BoolAND"
      }
      case x: BoolOR.type => {
        def ensure_constructible(): BoolOR.type = BoolOR
        s"BoolOR"
      }
      case x: BoolIMPLIES.type => {
        def ensure_constructible(): BoolIMPLIES.type = BoolIMPLIES
        s"BoolIMPLIES"
      }
      case x: BoolEQUIV.type => {
        def ensure_constructible(): BoolEQUIV.type = BoolEQUIV
        s"BoolEQUIV"
      }
    }
    case x: BVBinOp => x match {
      case x: BVAND.type => {
        def ensure_constructible(): BVAND.type = BVAND
        s"BVAND"
      }
      case x: BVOR.type => {
        def ensure_constructible(): BVOR.type = BVOR
        s"BVOR"
      }
      case x: BVADD.type => {
        def ensure_constructible(): BVADD.type = BVADD
        s"BVADD"
      }
      case x: BVMUL.type => {
        def ensure_constructible(): BVMUL.type = BVMUL
        s"BVMUL"
      }
      case x: BVUDIV.type => {
        def ensure_constructible(): BVUDIV.type = BVUDIV
        s"BVUDIV"
      }
      case x: BVUREM.type => {
        def ensure_constructible(): BVUREM.type = BVUREM
        s"BVUREM"
      }
      case x: BVSHL.type => {
        def ensure_constructible(): BVSHL.type = BVSHL
        s"BVSHL"
      }
      case x: BVLSHR.type => {
        def ensure_constructible(): BVLSHR.type = BVLSHR
        s"BVLSHR"
      }
      case x: BVULT.type => {
        def ensure_constructible(): BVULT.type = BVULT
        s"BVULT"
      }
      case x: BVNAND.type => {
        def ensure_constructible(): BVNAND.type = BVNAND
        s"BVNAND"
      }
      case x: BVNOR.type => {
        def ensure_constructible(): BVNOR.type = BVNOR
        s"BVNOR"
      }
      case x: BVXOR.type => {
        def ensure_constructible(): BVXOR.type = BVXOR
        s"BVXOR"
      }
      case x: BVXNOR.type => {
        def ensure_constructible(): BVXNOR.type = BVXNOR
        s"BVXNOR"
      }
      case x: BVCOMP.type => {
        def ensure_constructible(): BVCOMP.type = BVCOMP
        s"BVCOMP"
      }
      case x: BVSUB.type => {
        def ensure_constructible(): BVSUB.type = BVSUB
        s"BVSUB"
      }
      case x: BVSDIV.type => {
        def ensure_constructible(): BVSDIV.type = BVSDIV
        s"BVSDIV"
      }
      case x: BVSREM.type => {
        def ensure_constructible(): BVSREM.type = BVSREM
        s"BVSREM"
      }
      case x: BVSMOD.type => {
        def ensure_constructible(): BVSMOD.type = BVSMOD
        s"BVSMOD"
      }
      case x: BVASHR.type => {
        def ensure_constructible(): BVASHR.type = BVASHR
        s"BVASHR"
      }
      case x: BVULE.type => {
        def ensure_constructible(): BVULE.type = BVULE
        s"BVULE"
      }
      case x: BVUGT.type => {
        def ensure_constructible(): BVUGT.type = BVUGT
        s"BVUGT"
      }
      case x: BVUGE.type => {
        def ensure_constructible(): BVUGE.type = BVUGE
        s"BVUGE"
      }
      case x: BVSLT.type => {
        def ensure_constructible(): BVSLT.type = BVSLT
        s"BVSLT"
      }
      case x: BVSLE.type => {
        def ensure_constructible(): BVSLE.type = BVSLE
        s"BVSLE"
      }
      case x: BVSGT.type => {
        def ensure_constructible(): BVSGT.type = BVSGT
        s"BVSGT"
      }
      case x: BVSGE.type => {
        def ensure_constructible(): BVSGE.type = BVSGE
        s"BVSGE"
      }
      case x: BVEQ.type => {
        def ensure_constructible(): BVEQ.type = BVEQ
        s"BVEQ"
      }
      case x: BVNEQ.type => {
        def ensure_constructible(): BVNEQ.type = BVNEQ
        s"BVNEQ"
      }
      case x: BVCONCAT.type => {
        def ensure_constructible(): BVCONCAT.type = BVCONCAT
        s"BVCONCAT"
      }
    }
    case x: IntBinOp => x match {
      case x: IntADD.type => {
        def ensure_constructible(): IntADD.type = IntADD
        s"IntADD"
      }
      case x: IntMUL.type => {
        def ensure_constructible(): IntMUL.type = IntMUL
        s"IntMUL"
      }
      case x: IntSUB.type => {
        def ensure_constructible(): IntSUB.type = IntSUB
        s"IntSUB"
      }
      case x: IntDIV.type => {
        def ensure_constructible(): IntDIV.type = IntDIV
        s"IntDIV"
      }
      case x: IntMOD.type => {
        def ensure_constructible(): IntMOD.type = IntMOD
        s"IntMOD"
      }
      case x: IntEQ.type => {
        def ensure_constructible(): IntEQ.type = IntEQ
        s"IntEQ"
      }
      case x: IntNEQ.type => {
        def ensure_constructible(): IntNEQ.type = IntNEQ
        s"IntNEQ"
      }
      case x: IntLT.type => {
        def ensure_constructible(): IntLT.type = IntLT
        s"IntLT"
      }
      case x: IntLE.type => {
        def ensure_constructible(): IntLE.type = IntLE
        s"IntLE"
      }
      case x: IntGT.type => {
        def ensure_constructible(): IntGT.type = IntGT
        s"IntGT"
      }
      case x: IntGE.type => {
        def ensure_constructible(): IntGE.type = IntGE
        s"IntGE"
      }
    }
  }

given ToScala[Endian] with
  extension (x: Endian) def toScala: String = x match {
    case x: Endian.LittleEndian.type => {
      def ensure_constructible(): Endian.LittleEndian.type = Endian.LittleEndian
      s"Endian.LittleEndian"
    }
    case x: Endian.BigEndian.type => {
      def ensure_constructible(): Endian.BigEndian.type = Endian.BigEndian
      s"Endian.BigEndian"
    }
  }

given ToScala[Global] with
  extension (x: Global) def toScala: String = x match {
    case x: Register => {
      def ensure_constructible(): Register = Register(x.name, x.size)
      s"Register(${x.name.toScala}, ${x.size.toScala})"
    }
    case x: Memory => x match {
      case x: StackMemory => {
        def ensure_constructible(): StackMemory = StackMemory(x.name, x.addressSize, x.valueSize)
        s"StackMemory(${x.name.toScala}, ${x.addressSize.toScala}, ${x.valueSize.toScala})"
      }
      case x: SharedMemory => {
        def ensure_constructible(): SharedMemory = SharedMemory(x.name, x.addressSize, x.valueSize)
        s"SharedMemory(${x.name.toScala}, ${x.addressSize.toScala}, ${x.valueSize.toScala})"
      }
    }
  }

// end generated from ./expr.json

// generated from ./statements.json
given ToScala[Command] with
  extension (x: Command) def toScala: String = x match {
    case x: Statement => x match {
      case x: Assign => x match {
        case x: SingleAssign => x match {
          case x: LocalAssign => {
            def ensure_constructible(): LocalAssign = LocalAssign(x.lhs, x.rhs, x.label)
            s"LocalAssign(${x.lhs.toScala}, ${x.rhs.toScala}, ${x.label.toScala})"
          }
          case x: MemoryLoad => {
            def ensure_constructible(): MemoryLoad = MemoryLoad(x.lhs, x.mem, x.index, x.endian, x.size, x.label)
            s"MemoryLoad(${x.lhs.toScala}, ${x.mem.toScala}, ${x.index.toScala}, ${x.endian.toScala}, ${x.size.toScala}, ${x.label.toScala})"
          }
        }
        case x: DirectCall => {
          def ensure_constructible(): DirectCall = DirectCall(x.target, x.label, x.outParams, x.actualParams)
          if (Thread.interrupted()) { Thread.currentThread().interrupt(); "<interrupted>" } else summon[ToScala[DirectCall]].toScala(x)
        }
      }
      case x: MemoryStore => {
        def ensure_constructible(): MemoryStore = MemoryStore(x.mem, x.index, x.value, x.endian, x.size, x.label)
        s"MemoryStore(${x.mem.toScala}, ${x.index.toScala}, ${x.value.toScala}, ${x.endian.toScala}, ${x.size.toScala}, ${x.label.toScala})"
      }
      case x: NOP => {
        def ensure_constructible(): NOP = NOP(x.label)
        s"NOP(${x.label.toScala})"
      }
      case x: Assert => {
        def ensure_constructible(): Assert = Assert(x.body, x.comment, x.label)
        s"Assert(${x.body.toScala}, ${x.comment.toScala}, ${x.label.toScala})"
      }
      case x: Assume => {
        def ensure_constructible(): Assume = Assume(x.body, x.comment, x.label, x.checkSecurity)
        s"Assume(${x.body.toScala}, ${x.comment.toScala}, ${x.label.toScala}, ${x.checkSecurity.toScala})"
      }
      case x: Call => x match {
        case x: DirectCall => {
          def ensure_constructible(): DirectCall = DirectCall(x.target, x.label, x.outParams, x.actualParams)
          if (Thread.interrupted()) { Thread.currentThread().interrupt(); "<interrupted>" } else summon[ToScala[DirectCall]].toScala(x)
        }
        case x: IndirectCall => {
          def ensure_constructible(): IndirectCall = IndirectCall(x.target, x.label)
          if (Thread.interrupted()) { Thread.currentThread().interrupt(); "<interrupted>" } else summon[ToScala[IndirectCall]].toScala(x)
        }
      }
    }
    case x: Jump => x match {
      case x: Unreachable => {
        def ensure_constructible(): Unreachable = Unreachable(x.label)
        s"Unreachable(${x.label.toScala})"
      }
      case x: Return => {
        def ensure_constructible(): Return = Return(x.label, x.outParams)
        if (Thread.interrupted()) { Thread.currentThread().interrupt(); "<interrupted>" } else summon[ToScala[Return]].toScala(x)
      }
      case x: GoTo => {
        // unable to validate constructor with private field:
        // def ensure_constructible(): GoTo = GoTo(x._targets, x.label)
        if (Thread.interrupted()) { Thread.currentThread().interrupt(); "<interrupted>" } else summon[ToScala[GoTo]].toScala(x)
      }
    }
  }

// end generated from ./statements.json

// generated from ./irtype.json
given ToScala[IRType] with
  extension (x: IRType) def toScala: String = x match {
    case x: BoolType.type => {
      def ensure_constructible(): BoolType.type = BoolType
      s"BoolType"
    }
    case x: IntType.type => {
      def ensure_constructible(): IntType.type = IntType
      s"IntType"
    }
    case x: BitVecType => {
      def ensure_constructible(): BitVecType = BitVecType(x.size)
      s"BitVecType(${x.size.toScala})"
    }
    case x: MapType => {
      def ensure_constructible(): MapType = MapType(x.param, x.result)
      s"MapType(${x.param.toScala}, ${x.result.toScala})"
    }
  }

// end generated from ./irtype.json

// format: on
