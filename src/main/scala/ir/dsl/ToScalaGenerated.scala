package ir.dsl

import ir.*
import util.{Twine, indentNested}

/**
 * The end of this file contains generated code to implement ToScalaLines for the various type
 * hierarchies defined by BASIL.
 */

/**
 * XXX: ATTENTION: auto-generated code!
 * ====================================
 *
 * If you are here to manually fix a compilation error, please make sure that
 * you understand the context and carefully apply a specific fix. Make sure
 * that the code maintains the general contract of the ToScalaLines trait.
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
 *     ./mill test.testOnly 'ir.ToScalaLinesTest'
 *
 * 9. Upload the changes.
 *
 */


/**
 * Manually-defined ToScalaLines instances
 * ----------------------------------
 *
 * Externals: DirectCall, IndirectCall, GoTo, Return
 *
 * The "Externals" line above tells the generator the following types as having
 * manually-defined ToScalaLines instances. These types interact with control-flow,
 * and the naive auto-generated code would be large and possibly cyclic.
 */


// NOTE: It is important that these handwritten given instances live in the same
// file as the generated code. These instances must be locatable by summon[],
// otherwise the generated code will self-recurse, leading to non-termination.

given ToScalaLines[Return] with
  extension (x: Return)
    def toScalaLines =
      def outParamToScalaLines(x: (LocalVar, Expr)) = (x(0).name, x(1))

      if (x.outParams.isEmpty) {
        LazyList("ret")
      } else {
        indentNested(
          "ret(",
          x.outParams.map(outParamToScalaLines).map(_.toScala).map(LazyList(_)),
          ")")
      }

given ToScalaLines[DirectCall] with
  extension (x: DirectCall) def toScalaLines =
    LazyList(s"directCall(${x.target.procName.toScala})")

given ToScalaLines[IndirectCall] with
  extension (x: IndirectCall) def toScalaLines =
    LazyList(s"indirectCall(${x.target.toScala})")

given ToScalaLines[GoTo] with
  extension (x: GoTo) def toScalaLines =
    LazyList(s"goto(${x.targets.map(x => x.label.toScala).mkString(", ")})")


// WARNING: Everything below the next line will be overwritten by the generator!
// ------------------------ >8 ------------------------

// format: off

// command:
// scripts/make_repr_functions.py src/main/scala/ir/dsl/ToScalaGenerated.scala ./expr.json ./statements.json ./irtype.json

// generated from ./expr.json
given ToScalaLines[Expr] with
  extension (x: Expr) def toScalaLines: Twine = x match {
    case x: Literal => x match {
      case x: BoolLit => x match {
        case x: TrueLiteral.type => {
          def ensure_constructible(): TrueLiteral.type = TrueLiteral
          LazyList(s"TrueLiteral")
        }
        case x: FalseLiteral.type => {
          def ensure_constructible(): FalseLiteral.type = FalseLiteral
          LazyList(s"FalseLiteral")
        }
      }
      case x: BitVecLiteral => {
        def ensure_constructible(): BitVecLiteral = BitVecLiteral(x.value, x.size)
        LazyList(s"BitVecLiteral(${x.value.toScala}, ${x.size.toScala})")
      }
      case x: IntLiteral => {
        def ensure_constructible(): IntLiteral = IntLiteral(x.value)
        LazyList(s"IntLiteral(${x.value.toScala})")
      }
    }
    case x: Extract => {
      def ensure_constructible(): Extract = Extract(x.end, x.start, x.body)
      LazyList(s"Extract(${x.end.toScala}, ${x.start.toScala}, ${x.body.toScala})")
    }
    case x: Repeat => {
      def ensure_constructible(): Repeat = Repeat(x.repeats, x.body)
      LazyList(s"Repeat(${x.repeats.toScala}, ${x.body.toScala})")
    }
    case x: ZeroExtend => {
      def ensure_constructible(): ZeroExtend = ZeroExtend(x.extension, x.body)
      LazyList(s"ZeroExtend(${x.extension.toScala}, ${x.body.toScala})")
    }
    case x: SignExtend => {
      def ensure_constructible(): SignExtend = SignExtend(x.extension, x.body)
      LazyList(s"SignExtend(${x.extension.toScala}, ${x.body.toScala})")
    }
    case x: UnaryExpr => {
      def ensure_constructible(): UnaryExpr = UnaryExpr(x.op, x.arg)
      LazyList(s"UnaryExpr(${x.op.toScala}, ${x.arg.toScala})")
    }
    case x: BinaryExpr => {
      def ensure_constructible(): BinaryExpr = BinaryExpr(x.op, x.arg1, x.arg2)
      LazyList(s"BinaryExpr(${x.op.toScala}, ${x.arg1.toScala}, ${x.arg2.toScala})")
    }
    case x: UninterpretedFunction => {
      def ensure_constructible(): UninterpretedFunction = UninterpretedFunction(x.name, x.params, x.returnType)
      LazyList(s"UninterpretedFunction(${x.name.toScala}, ${x.params.toScala}, ${x.returnType.toScala})")
    }
    case x: Variable => x match {
      case x: Register => {
        def ensure_constructible(): Register = Register(x.name, x.size)
        LazyList(s"Register(${x.name.toScala}, ${x.size.toScala})")
      }
      case x: LocalVar => {
        def ensure_constructible(): LocalVar = LocalVar(x.varName, x.irType, x.index)
        LazyList(s"LocalVar(${x.varName.toScala}, ${x.irType.toScala}, ${x.index.toScala})")
      }
    }
  }

given ToScalaLines[UnOp] with
  extension (x: UnOp) def toScalaLines: Twine = x match {
    case x: BoolUnOp => x match {
      case x: BoolNOT.type => {
        def ensure_constructible(): BoolNOT.type = BoolNOT
        LazyList(s"BoolNOT")
      }
      case x: BoolToBV1.type => {
        def ensure_constructible(): BoolToBV1.type = BoolToBV1
        LazyList(s"BoolToBV1")
      }
    }
    case x: IntUnOp => x match {
      case x: IntNEG.type => {
        def ensure_constructible(): IntNEG.type = IntNEG
        LazyList(s"IntNEG")
      }
    }
    case x: BVUnOp => x match {
      case x: BVNOT.type => {
        def ensure_constructible(): BVNOT.type = BVNOT
        LazyList(s"BVNOT")
      }
      case x: BVNEG.type => {
        def ensure_constructible(): BVNEG.type = BVNEG
        LazyList(s"BVNEG")
      }
    }
  }

given ToScalaLines[BinOp] with
  extension (x: BinOp) def toScalaLines: Twine = x match {
    case x: BoolBinOp => x match {
      case x: BoolEQ.type => {
        def ensure_constructible(): BoolEQ.type = BoolEQ
        LazyList(s"BoolEQ")
      }
      case x: BoolNEQ.type => {
        def ensure_constructible(): BoolNEQ.type = BoolNEQ
        LazyList(s"BoolNEQ")
      }
      case x: BoolAND.type => {
        def ensure_constructible(): BoolAND.type = BoolAND
        LazyList(s"BoolAND")
      }
      case x: BoolOR.type => {
        def ensure_constructible(): BoolOR.type = BoolOR
        LazyList(s"BoolOR")
      }
      case x: BoolIMPLIES.type => {
        def ensure_constructible(): BoolIMPLIES.type = BoolIMPLIES
        LazyList(s"BoolIMPLIES")
      }
      case x: BoolEQUIV.type => {
        def ensure_constructible(): BoolEQUIV.type = BoolEQUIV
        LazyList(s"BoolEQUIV")
      }
    }
    case x: BVBinOp => x match {
      case x: BVAND.type => {
        def ensure_constructible(): BVAND.type = BVAND
        LazyList(s"BVAND")
      }
      case x: BVOR.type => {
        def ensure_constructible(): BVOR.type = BVOR
        LazyList(s"BVOR")
      }
      case x: BVADD.type => {
        def ensure_constructible(): BVADD.type = BVADD
        LazyList(s"BVADD")
      }
      case x: BVMUL.type => {
        def ensure_constructible(): BVMUL.type = BVMUL
        LazyList(s"BVMUL")
      }
      case x: BVUDIV.type => {
        def ensure_constructible(): BVUDIV.type = BVUDIV
        LazyList(s"BVUDIV")
      }
      case x: BVUREM.type => {
        def ensure_constructible(): BVUREM.type = BVUREM
        LazyList(s"BVUREM")
      }
      case x: BVSHL.type => {
        def ensure_constructible(): BVSHL.type = BVSHL
        LazyList(s"BVSHL")
      }
      case x: BVLSHR.type => {
        def ensure_constructible(): BVLSHR.type = BVLSHR
        LazyList(s"BVLSHR")
      }
      case x: BVULT.type => {
        def ensure_constructible(): BVULT.type = BVULT
        LazyList(s"BVULT")
      }
      case x: BVNAND.type => {
        def ensure_constructible(): BVNAND.type = BVNAND
        LazyList(s"BVNAND")
      }
      case x: BVNOR.type => {
        def ensure_constructible(): BVNOR.type = BVNOR
        LazyList(s"BVNOR")
      }
      case x: BVXOR.type => {
        def ensure_constructible(): BVXOR.type = BVXOR
        LazyList(s"BVXOR")
      }
      case x: BVXNOR.type => {
        def ensure_constructible(): BVXNOR.type = BVXNOR
        LazyList(s"BVXNOR")
      }
      case x: BVCOMP.type => {
        def ensure_constructible(): BVCOMP.type = BVCOMP
        LazyList(s"BVCOMP")
      }
      case x: BVSUB.type => {
        def ensure_constructible(): BVSUB.type = BVSUB
        LazyList(s"BVSUB")
      }
      case x: BVSDIV.type => {
        def ensure_constructible(): BVSDIV.type = BVSDIV
        LazyList(s"BVSDIV")
      }
      case x: BVSREM.type => {
        def ensure_constructible(): BVSREM.type = BVSREM
        LazyList(s"BVSREM")
      }
      case x: BVSMOD.type => {
        def ensure_constructible(): BVSMOD.type = BVSMOD
        LazyList(s"BVSMOD")
      }
      case x: BVASHR.type => {
        def ensure_constructible(): BVASHR.type = BVASHR
        LazyList(s"BVASHR")
      }
      case x: BVULE.type => {
        def ensure_constructible(): BVULE.type = BVULE
        LazyList(s"BVULE")
      }
      case x: BVUGT.type => {
        def ensure_constructible(): BVUGT.type = BVUGT
        LazyList(s"BVUGT")
      }
      case x: BVUGE.type => {
        def ensure_constructible(): BVUGE.type = BVUGE
        LazyList(s"BVUGE")
      }
      case x: BVSLT.type => {
        def ensure_constructible(): BVSLT.type = BVSLT
        LazyList(s"BVSLT")
      }
      case x: BVSLE.type => {
        def ensure_constructible(): BVSLE.type = BVSLE
        LazyList(s"BVSLE")
      }
      case x: BVSGT.type => {
        def ensure_constructible(): BVSGT.type = BVSGT
        LazyList(s"BVSGT")
      }
      case x: BVSGE.type => {
        def ensure_constructible(): BVSGE.type = BVSGE
        LazyList(s"BVSGE")
      }
      case x: BVEQ.type => {
        def ensure_constructible(): BVEQ.type = BVEQ
        LazyList(s"BVEQ")
      }
      case x: BVNEQ.type => {
        def ensure_constructible(): BVNEQ.type = BVNEQ
        LazyList(s"BVNEQ")
      }
      case x: BVCONCAT.type => {
        def ensure_constructible(): BVCONCAT.type = BVCONCAT
        LazyList(s"BVCONCAT")
      }
    }
    case x: IntBinOp => x match {
      case x: IntADD.type => {
        def ensure_constructible(): IntADD.type = IntADD
        LazyList(s"IntADD")
      }
      case x: IntMUL.type => {
        def ensure_constructible(): IntMUL.type = IntMUL
        LazyList(s"IntMUL")
      }
      case x: IntSUB.type => {
        def ensure_constructible(): IntSUB.type = IntSUB
        LazyList(s"IntSUB")
      }
      case x: IntDIV.type => {
        def ensure_constructible(): IntDIV.type = IntDIV
        LazyList(s"IntDIV")
      }
      case x: IntMOD.type => {
        def ensure_constructible(): IntMOD.type = IntMOD
        LazyList(s"IntMOD")
      }
      case x: IntEQ.type => {
        def ensure_constructible(): IntEQ.type = IntEQ
        LazyList(s"IntEQ")
      }
      case x: IntNEQ.type => {
        def ensure_constructible(): IntNEQ.type = IntNEQ
        LazyList(s"IntNEQ")
      }
      case x: IntLT.type => {
        def ensure_constructible(): IntLT.type = IntLT
        LazyList(s"IntLT")
      }
      case x: IntLE.type => {
        def ensure_constructible(): IntLE.type = IntLE
        LazyList(s"IntLE")
      }
      case x: IntGT.type => {
        def ensure_constructible(): IntGT.type = IntGT
        LazyList(s"IntGT")
      }
      case x: IntGE.type => {
        def ensure_constructible(): IntGE.type = IntGE
        LazyList(s"IntGE")
      }
    }
  }

given ToScalaLines[Endian] with
  extension (x: Endian) def toScalaLines: Twine = x match {
    case x: Endian.LittleEndian.type => {
      def ensure_constructible(): Endian.LittleEndian.type = Endian.LittleEndian
      LazyList(s"Endian.LittleEndian")
    }
    case x: Endian.BigEndian.type => {
      def ensure_constructible(): Endian.BigEndian.type = Endian.BigEndian
      LazyList(s"Endian.BigEndian")
    }
  }

given ToScalaLines[Global] with
  extension (x: Global) def toScalaLines: Twine = x match {
    case x: Register => {
      def ensure_constructible(): Register = Register(x.name, x.size)
      LazyList(s"Register(${x.name.toScala}, ${x.size.toScala})")
    }
    case x: Memory => x match {
      case x: StackMemory => {
        def ensure_constructible(): StackMemory = StackMemory(x.name, x.addressSize, x.valueSize)
        LazyList(s"StackMemory(${x.name.toScala}, ${x.addressSize.toScala}, ${x.valueSize.toScala})")
      }
      case x: SharedMemory => {
        def ensure_constructible(): SharedMemory = SharedMemory(x.name, x.addressSize, x.valueSize)
        LazyList(s"SharedMemory(${x.name.toScala}, ${x.addressSize.toScala}, ${x.valueSize.toScala})")
      }
    }
  }

// end generated from ./expr.json

// generated from ./statements.json
given ToScalaLines[Command] with
  extension (x: Command) def toScalaLines: Twine = x match {
    case x: Statement => x match {
      case x: Assign => x match {
        case x: SingleAssign => x match {
          case x: LocalAssign => {
            def ensure_constructible(): LocalAssign = LocalAssign(x.lhs, x.rhs, x.label)
            LazyList(s"LocalAssign(${x.lhs.toScala}, ${x.rhs.toScala}, ${x.label.toScala})")
          }
          case x: MemoryLoad => {
            def ensure_constructible(): MemoryLoad = MemoryLoad(x.lhs, x.mem, x.index, x.endian, x.size, x.label)
            LazyList(s"MemoryLoad(${x.lhs.toScala}, ${x.mem.toScala}, ${x.index.toScala}, ${x.endian.toScala}, ${x.size.toScala}, ${x.label.toScala})")
          }
        }
        case x: DirectCall => {
          def ensure_constructible(): DirectCall = DirectCall(x.target, x.label, x.outParams, x.actualParams)
          if (Thread.interrupted()) { Thread.currentThread().interrupt(); LazyList("<interrupted>") } else summon[ToScalaLines[DirectCall]].toScalaLines(x)
        }
      }
      case x: MemoryStore => {
        def ensure_constructible(): MemoryStore = MemoryStore(x.mem, x.index, x.value, x.endian, x.size, x.label)
        LazyList(s"MemoryStore(${x.mem.toScala}, ${x.index.toScala}, ${x.value.toScala}, ${x.endian.toScala}, ${x.size.toScala}, ${x.label.toScala})")
      }
      case x: NOP => {
        def ensure_constructible(): NOP = NOP(x.label)
        LazyList(s"NOP(${x.label.toScala})")
      }
      case x: Assert => {
        def ensure_constructible(): Assert = Assert(x.body, x.comment, x.label)
        LazyList(s"Assert(${x.body.toScala}, ${x.comment.toScala}, ${x.label.toScala})")
      }
      case x: Assume => {
        def ensure_constructible(): Assume = Assume(x.body, x.comment, x.label, x.checkSecurity)
        LazyList(s"Assume(${x.body.toScala}, ${x.comment.toScala}, ${x.label.toScala}, ${x.checkSecurity.toScala})")
      }
      case x: Call => x match {
        case x: DirectCall => {
          def ensure_constructible(): DirectCall = DirectCall(x.target, x.label, x.outParams, x.actualParams)
          if (Thread.interrupted()) { Thread.currentThread().interrupt(); LazyList("<interrupted>") } else summon[ToScalaLines[DirectCall]].toScalaLines(x)
        }
        case x: IndirectCall => {
          def ensure_constructible(): IndirectCall = IndirectCall(x.target, x.label)
          if (Thread.interrupted()) { Thread.currentThread().interrupt(); LazyList("<interrupted>") } else summon[ToScalaLines[IndirectCall]].toScalaLines(x)
        }
      }
    }
    case x: Jump => x match {
      case x: Unreachable => {
        def ensure_constructible(): Unreachable = Unreachable(x.label)
        LazyList(s"Unreachable(${x.label.toScala})")
      }
      case x: Return => {
        def ensure_constructible(): Return = Return(x.label, x.outParams)
        if (Thread.interrupted()) { Thread.currentThread().interrupt(); LazyList("<interrupted>") } else summon[ToScalaLines[Return]].toScalaLines(x)
      }
      case x: GoTo => {
        // unable to validate constructor with private field:
        // def ensure_constructible(): GoTo = GoTo(x._targets, x.label)
        if (Thread.interrupted()) { Thread.currentThread().interrupt(); LazyList("<interrupted>") } else summon[ToScalaLines[GoTo]].toScalaLines(x)
      }
    }
  }

// end generated from ./statements.json

// generated from ./irtype.json
given ToScalaLines[IRType] with
  extension (x: IRType) def toScalaLines: Twine = x match {
    case x: BoolType.type => {
      def ensure_constructible(): BoolType.type = BoolType
      LazyList(s"BoolType")
    }
    case x: IntType.type => {
      def ensure_constructible(): IntType.type = IntType
      LazyList(s"IntType")
    }
    case x: BitVecType => {
      def ensure_constructible(): BitVecType = BitVecType(x.size)
      LazyList(s"BitVecType(${x.size.toScala})")
    }
    case x: MapType => {
      def ensure_constructible(): MapType = MapType(x.param, x.result)
      LazyList(s"MapType(${x.param.toScala}, ${x.result.toScala})")
    }
  }

// end generated from ./irtype.json

// format: on
