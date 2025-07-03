package ir.eval
import boogie.Scope
import ir.*
import util.Logger
import util.functional.*
import util.functional.State.*

import scala.collection.immutable

/** Abstraction for memload and variable lookup used by the expression evaluator.
  */
case class StVarLoader[S, F <: Effects[S, InterpreterError]](f: F) extends Loader[S, InterpreterError] {

  def getVariable(v: Variable): State[S, Option[Literal], InterpreterError] = {
    for {
      v <- f.loadVar(v.name)
    } yield ((v match {
      case Scalar(l) => Some(l)
      case _ => None
    }))
  }

  override def loadMemory(
    m: Memory,
    addr: Expr,
    endian: Endian,
    size: Int
  ): State[S, Option[Literal], InterpreterError] = {
    for {
      r <- addr match {
        case l: Literal if size == 1 =>
          Eval
            .loadSingle(f)(m.name, Scalar(l))
            .map((v: BasilValue) =>
              v match {
                case Scalar(l) => Some(l)
                case _ => None
              }
            )
        case l: Literal => Eval.loadBV(f)(m.name, Scalar(l), endian, size).map(Some(_))
        case _ => get((s: S) => None)
      }
    } yield (r)
  }

}

/*
 * Helper functions for compiling high level structures to the interpreter effects.
 * All are parametric in concrete state S and Effects[S]
 */
case object Eval {

  /*--------------------------------------------------------------------------------*/
  /* Eval functions                                                                 */
  /*--------------------------------------------------------------------------------*/

  // def evalExpr[S, T <: Effects[S, InterpreterError]](f: T)(e: Expr): State[S, Expr, InterpreterError] = {
  //   val ldr = StVarLoader[S, T](f)

  //   val varbs = e.variables.toList
  //   val vars = varbs.map(v => ldr.getVariable(v))

  //   for {
  //     vs <- State.mapM(ldr.getVariable, varbs)
  //     vvs = varbs.zip(vs).filter(_._2.isDefined).map(l => (l._1, l._2.get)).toMap
  //     substed = Substitute(vvs.get)(e).flatMap(evaluateExpr)
  //     res <- substed match {
  //       case Some(r) => State.pure(r)
  //       case None    => State.pure(e)
  //     }
  //   } yield (res)
  // }
  //

  def evalExpr[S, T <: Effects[S, InterpreterError]](f: T)(e: Expr): State[S, Expr, InterpreterError] = {
    val ldr = StVarLoader[S, T](f)
    for {
      res <- ir.eval.statePartialEvalExpr[S](ldr)(e)
    } yield (res)
  }

  def evalLiteral[S, T <: Effects[S, InterpreterError]](f: T)(e: Expr): State[S, Literal, InterpreterError] = {
    for {
      res <- evalExpr(f)(e)
      r <- State.pureE(res match {
        case l: Literal => Right(l)
        case _ => Left((Errored(s"Eval BV residual $e")))
      })
    } yield (r)
  }

  def evalBV[S, T <: Effects[S, InterpreterError]](f: T)(e: Expr): State[S, BitVecLiteral, InterpreterError] = {
    for {
      res <- evalExpr(f)(e)
      r <- State.pureE(res match {
        case l: BitVecLiteral => Right(l)
        case _ => Left((Errored(s"Eval BV residual $e")))
      })
    } yield (r)
  }

  def evalInt[S, T <: Effects[S, InterpreterError]](f: T)(e: Expr): State[S, BigInt, InterpreterError] = {
    for {
      res <- evalExpr(f)(e)
      r <- State.pureE(res match {
        case l: IntLiteral => Right(l.value)
        case _ => Left((Errored(s"Eval Int residual $e")))
      })
    } yield (r)
  }

  def evalBool[S, T <: Effects[S, InterpreterError]](f: T)(e: Expr): State[S, Boolean, InterpreterError] = {
    for {
      res <- evalExpr(f)(e)
      r <- State.pureE(res match {
        case l: BoolLit => Right(l == TrueLiteral)
        case _ => Left((Errored(s"Eval Bool residual $e")))
      })
    } yield (r)
  }

  /*--------------------------------------------------------------------------------*/
  /* Load functions                                                                 */
  /*--------------------------------------------------------------------------------*/

  def load[S, T <: Effects[S, InterpreterError]](
    f: T
  )(vname: String, addr: Scalar, endian: Endian, count: Int): State[S, List[BasilValue], InterpreterError] = {
    for {
      _ <-
        if (count == 0) then State.setError((Errored(s"Attempted fractional load"))) else State.pure(())
      keys <- State.mapM(((i: Int) => State.pureE(BasilValue.unsafeAdd(addr, i))), (0 until count))
      values <- f.loadMem(vname, keys.toList)
      vals = endian match {
        case Endian.LittleEndian => values.reverse
        case Endian.BigEndian => values
      }
    } yield (vals.toList)
  }

  /** Load and concat bitvectors */
  def loadBV[S, T <: Effects[S, InterpreterError]](
    f: T
  )(vname: String, addr: Scalar, endian: Endian, size: Int): State[S, BitVecLiteral, InterpreterError] = for {
    mem <- f.loadVar(vname)
    x <- mem match {
      case mapv @ BasilMapValue(_, MapType(_, BitVecType(sz))) => State.pure((sz, mapv))
      case _ => State.setError((Errored("Trued to load-concat non bv")))
    }
    (valsize, mapv) = x

    cells = size / valsize

    res <- load(f)(vname, addr, endian, cells) // actual load
    bvs: List[BitVecLiteral] <- (
      State.mapM(
        (c: BasilValue) =>
          c match {
            case Scalar(bv @ BitVecLiteral(v, sz)) if sz == valsize => State.pure(bv)
            case c =>
              State.setError(TypeError(s"Loaded value of type ${c.irType} did not match expected type bv$valsize"))
          },
        res
      )
    )
  } yield (bvs.foldLeft(BitVecLiteral(0, 0))((acc, r) => eval.evalBVBinExpr(BVCONCAT, acc, r)))

  def loadSingle[S, T <: Effects[S, InterpreterError]](
    f: T
  )(vname: String, addr: Scalar): State[S, BasilValue, InterpreterError] = {
    for {
      m <- load(f)(vname, addr, Endian.LittleEndian, 1)
    } yield (m.head)
  }

  /*--------------------------------------------------------------------------------*/
  /* Store functions                                                                */
  /*--------------------------------------------------------------------------------*/

  /* Expand addr for number of values to store */
  def store[S, T <: Effects[S, InterpreterError]](
    f: T
  )(vname: String, addr: BasilValue, values: List[BasilValue], endian: Endian)(implicit
    line: sourcecode.Line,
    file: sourcecode.FileName,
    name: sourcecode.Name
  ): State[S, Unit, InterpreterError] =
    monlog.debug(s"store ${vname} ${values.size} bytes ${file.value}:${line.value}")
    for {
      mem <- f.loadVar(vname)
      x <- mem match {
        case m @ BasilMapValue(_, MapType(kt, vt))
            if Some(kt) == addr.irType && values.forall(v => v.irType == Some(vt)) =>
          State.pure((m, kt, vt))
        case v => State.setError((TypeError(s"Invalid map store operation to $vname : $v")))
      }
      (mapval, keytype, valtype) = x
      keys <- State.mapM((i: Int) => State.pureE(BasilValue.unsafeAdd(addr, i)), (0 until values.size))
      vals = endian match {
        case Endian.LittleEndian => values.reverse
        case Endian.BigEndian => values
      }
      x <- f.storeMem(vname, keys.zip(vals).toMap)
    } yield (x)

  /** Extract bitvec to bytes and store bytes */
  def storeBV[S, T <: Effects[S, InterpreterError]](
    f: T
  )(vname: String, addr: BasilValue, value: BitVecLiteral, endian: Endian)(implicit
    line: sourcecode.Line,
    file: sourcecode.FileName,
    name: sourcecode.Name
  ): State[S, Unit, InterpreterError] =

    monlog.debug(s"storeBV ${vname} ${size(value).get / 8} bytes")(line, file, name)
    for {
      mem <- f.loadVar(vname)
      mr <- mem match {
        case m @ BasilMapValue(_, MapType(kt, BitVecType(size))) if Some(kt) == addr.irType => State.pure((m, size))
        case v =>
          State.setError(
            TypeError(
              s"Invalid map store operation to $vname : ${v.irType} (expect [${addr.irType}] <- ${value.getType})"
            )
          )
      }
      (mapval, vsize) = mr
      cells = value.size / vsize
      _ = {
        if (cells < 1) {
          State.setError((MemoryError("Tried to execute fractional store")))
        } else {
          State.pure(())
        }
      }

      extractVals = (0 until cells).map(i => BitVectorEval.boogie_extract((i + 1) * vsize, i * vsize, value)).toList
      vs = endian match {
        case Endian.LittleEndian => extractVals.map(Scalar(_))
        case Endian.BigEndian => extractVals.reverse.map(Scalar(_))
      }

      keys <- State.mapM((i: Int) => State.pureE(BasilValue.unsafeAdd(addr, i)), (0 until cells))
      s <- f.storeMem(vname, keys.zip(vs).toMap)
    } yield (s)

  def storeSingle[S, E, T <: Effects[S, E]](
    f: T
  )(vname: String, addr: BasilValue, value: BasilValue): State[S, Unit, E] = {
    f.storeMem(vname, Map((addr -> value)))
  }

  /** Helper functions * */

  /** Load all memory cells from pointer until reaching cell containing 0. Ptr -> List[Bitvector]
    */
  def getNullTerminatedString[S, T <: Effects[S, InterpreterError]](
    f: T
  )(rgn: String, src: BasilValue, acc: List[BitVecLiteral] = List()): State[S, List[BitVecLiteral], InterpreterError] =
    for {
      srv: BitVecLiteral <- src match {
        case Scalar(b: BitVecLiteral) => State.pure(b)
        case _ => State.setError(Errored(s"Not pointer : $src"))
      }
      c <- f.loadMem(rgn, List(src))
      res <- c.head match {
        case Scalar(BitVecLiteral(0, 8)) => State.pure(acc)
        case Scalar(b: BitVecLiteral) => {
          for {
            nsrc <- State.pureE(BasilValue.unsafeAdd(src, 1))
            r <- getNullTerminatedString(f)(rgn, nsrc, acc.appended(b))
          } yield (r)
        }
        case _ => State.setError(Errored(s"not byte $c"))
      }
    } yield (res)

}

enum InterpretReturn {
  case ReturnVal(outs: Map[LocalVar, Literal])
  case Void
}

class BASILInterpreter[S](f: Effects[S, InterpreterError])
/*extends Interpreter[S, InterpretReturn, InterpreterError](f) */ {

  def interpretOne: util.functional.State[S, Next[InterpretReturn], InterpreterError] =
    InterpFuns.interpretContinuation(f)

}

object InterpFuns {

  def interpretContinuation[S, T <: Effects[S, InterpreterError]](
    f: T
  ): State[S, Next[InterpretReturn], InterpreterError] = {
    val next = for {
      next <- f.getNext
      _ <- State.pure(Logger.debug(s"$next"))
      r: Next[InterpretReturn] <- (next match {
        case Intrinsic(tgt) => callProcedure(f)(tgt, List()).map(ret => Next.Stop(InterpretReturn.ReturnVal(ret)))
        case Run(c: Statement) => interpretStatement(f)(c).map(_ => Next.Continue)
        case ReturnFrom(c) => evaluateReturn(f)(c).map(v => Next.Stop(InterpretReturn.ReturnVal(v)))
        case Run(c: Jump) => interpretJump(f)(c).map(_ => Next.Continue)
        case Stopped() => State.pure(Next.Stop(InterpretReturn.Void))
        case ErrorStop(e) => State.pure(Next.Stop(InterpretReturn.Void))
      })
    } yield (r)

    next.flatMapE((e: InterpreterError) => {
      f.setNext(ErrorStop(e)).map(_ => Next.Stop(InterpretReturn.Void))
    })
  }

  def interpretJump[S, T <: Effects[S, InterpreterError]](f: T)(j: Jump): State[S, Unit, InterpreterError] = {
    j match {
      case gt: GoTo if gt.targets.size == 1 => {
        f.setNext(Run(IRWalk.firstInBlock(gt.targets.head)))
      }
      case gt: GoTo =>
        val assumes = gt.targets.flatMap(_.statements.headOption).collect { case a: Assume =>
          a
        }
        for {
          _ <-
            if (assumes.size != gt.targets.size) {
              State.setError((Errored(s"Some goto target missing guard $gt")))
            } else {
              State.pure(())
            }
          chosen: List[Assume] <- filterM((a: Assume) => Eval.evalBool(f)(a.body), assumes)

          res <- chosen match {
            case Nil => State.setError(Errored(s"No jump target satisfied $gt"))
            case h :: Nil => f.setNext(Run(h))
            case h :: tl => State.setError(Errored(s"More than one jump guard satisfied $gt"))
          }
        } yield (res)
      case r: Return => {
        // eval return values, return to caller, then bind return values to formal out params
        for {
          outs <- State.mapM(
            ((bindout: (LocalVar, Expr)) => {
              for {
                rhs <- Eval.evalLiteral(f)(bindout._2)
              } yield (bindout._1, rhs)
            }),
            r.outParams
          )
          _ <- f.doReturn()
          _ <- State.sequence(State.pure(()), outs.map(m => f.storeVar(m._1.name, m._1.toBoogie.scope, Scalar(m._2))))
        } yield ()
      }
      case h: Unreachable => State.setError(EscapedControlFlow(h))
    }
  }

  /**
   * Evaluates the formal out params and returns a map totheir values.
   */
  def evaluateReturn[S, T <: Effects[S, InterpreterError]](
    f: T
  )(returnFrom: ProcSig): State[S, Map[LocalVar, Literal], InterpreterError] = {
    for {
      outs <- State.mapM(
        ((bindout: (LocalVar)) => {
          for {
            rhs <- Eval.evalLiteral(f)(bindout)
          } yield (bindout, rhs)
        }),
        returnFrom.formalOutParam
      )
    } yield (outs.toMap)
  }

  def interpretStatement[S, T <: Effects[S, InterpreterError]](f: T)(s: Statement): State[S, Unit, InterpreterError] = {
    s match {
      case assign: MemoryAssign => {
        for {
          rhs <- Eval.evalLiteral(f)(assign.rhs)
          st <- f.storeVar(assign.lhs.name, assign.lhs.toBoogie.scope, Scalar(rhs))
          n <- f.setNext(Run(s.successor))
        } yield (st)
      }
      case SimulAssign(assigns, _) => {
        val a = assigns.map((l, r) =>
          for {
            rhs <- Eval.evalLiteral(f)(r)
            st <- f.storeVar(l.name, l.toBoogie.scope, Scalar(rhs))
            n <- f.setNext(Run(s.successor))
          } yield (st)
        )
        State.sequence(State.pure(()), a)
      }
      case assign: MemoryLoad => {
        for {
          index <- Eval.evalBV(f)(assign.index)
          loaded <- Eval.loadBV(f)(assign.mem.name, Scalar(index), assign.endian, assign.size)
          st <- f.storeVar(assign.lhs.name, assign.lhs.toBoogie.scope, Scalar(loaded))
          n <- f.setNext(Run(s.successor))
        } yield (st)
      }
      case assign: MemoryStore =>
        for {
          index: BitVecLiteral <- Eval.evalBV(f)(assign.index)
          value: BitVecLiteral <- Eval.evalBV(f)(assign.value)
          _ <- Eval.storeBV(f)(assign.mem.name, Scalar(index), value, assign.endian)
          n <- f.setNext(Run(s.successor))
        } yield (n)
      case assert: Assert =>
        for {
          b <- Eval.evalBool(f)(assert.body)
          _ <-
            (if (!b) then {
               State.setError(FailedAssertion(assert))
             } else {
               f.setNext(Run(s.successor))
             })
        } yield ()
      case assume: Assume =>
        for {
          b <- Eval.evalBool(f)(assume.body)
          n <-
            (if (!b) {
               State.setError(Errored(s"Assumption not satisfied: $assume"))
             } else {
               f.setNext(Run(s.successor))
             })
        } yield (n)
      case dc: DirectCall => {
        for {
          // eval actual
          actualParams <- State.mapM(
            (p: (LocalVar, Expr)) =>
              for {
                v <- Eval.evalLiteral(f)(p._2)
              } yield (p._1, v),
            dc.actualParams
          )
          // call procedure (immediately evaluated)
          ret <- callProcedure(f)(dc.target, actualParams)
          // assign return values of procedure
          outs =
            dc.outParams.map { (formal: LocalVar, lhs: Variable) =>
              (formal, lhs, ret(formal))
            }
          _ <- State.sequence(
            State.pure(()),
            outs.map { (formal, lhs, value) =>
              f.storeVar(lhs.name, lhs.toBoogie.scope, Scalar(value))
            }
          )
          _ <- f.setNext(Run(s.successor))
        } yield ()
      }
      case ic: IndirectCall => {
        if (ic.target == Register("R30", 64)) {
          f.doReturn()
        } else {
          for {
            addr <- Eval.evalBV(f)(ic.target)
            block = ic.parent.parent.blocks.find(_.address.contains(addr.value))
            _ <- block match {
              case Some(b: Block) => {
                f.setNext(Run(IRWalk.firstInBlock(b)))
              }
              case None =>
                for {
                  fp <- f.evalAddrToProc(addr.value.toInt)
                  _ <- fp match {
                    case Some(fp) => f.call(fp.name, fp.call, Run(ic.successor))
                    case none => State.setError(EscapedControlFlow(ic))
                  }
                } yield (())
            }
          } yield ()
        }
      }
      case _: NOP => f.setNext(Run(s.successor))
    }
  }

  def initRelocTable[S, T <: Effects[S, InterpreterError]](s: T)(ctx: IRContext): State[S, Unit, InterpreterError] = {

    val p = ctx.program

    var addr = ctx.symbols.find(_.name == "__end__").map(_.value).getOrElse(BigInt(123456))
    var done = false
    var x = List[(String, FunPointer)]()

    def newAddr(): BigInt = {
      addr += 8
      addr
    }

    for ((fname, funSig) <- LibcIntrinsic.intrinsicSigs) {
      val name = fname.takeWhile(c => c != '@')
      x = (name, FunPointer(BitVecLiteral(newAddr(), 64), name, Intrinsic(funSig))) :: x
    }

    val intrinsics = x.toMap

    val procs = p.procedures.filter(proc => proc.address.isDefined)

    val fptrs = ctx.externalFunctions.toList
      .sortBy(_.name)
      .flatMap(f => {
        intrinsics
          .get(f.name)
          .map(fp => (f.offset, fp))
          .orElse(
            procs
              .find(p => p.name == f.name)
              .map(proc =>
                (
                  f.offset,
                  FunPointer(
                    BitVecLiteral(proc.address.getOrElse(newAddr().toInt), 64),
                    proc.name,
                    Run(DirectCall(proc))
                  )
                )
              )
          )
      })

    // sort for deterministic trace
    val stores = fptrs
      .sortBy(f => f._1)
      .map((p) => {
        val (offset, fptr) = p
        Eval.storeSingle(s)("ghost-funtable", Scalar(fptr.addr), fptr)
          >> (Eval.storeBV(s)("mem", Scalar(BitVecLiteral(offset, 64)), fptr.addr, Endian.LittleEndian))
      })

    for {
      _ <- State.sequence(State.pure(()), stores)
      malloc_top = BitVecLiteral(newAddr() + 1024, 64)
      _ <- s.storeVar("ghost_malloc_top", Scope.Global, Scalar(malloc_top))
    } yield (())
  }

  /** Functions which compile BASIL IR down to the minimal interpreter effects.
    *
    * Each function takes as parameter an implementation of Effects[S]
    */

  def initialState[S, E, T <: Effects[S, E]](s: T): State[S, Unit, E] = {
    val SP: BitVecLiteral = BitVecLiteral(0x78000000, 64)
    val FP: BitVecLiteral = SP
    val LR: BitVecLiteral = BitVecLiteral(BigInt("78000000", 16), 64)

    for {
      h <- State.pure(Logger.debug("DEFINE MEMORY REGIONS"))
      h <- s.storeVar("mem", Scope.Global, BasilMapValue(Map.empty, MapType(BitVecType(64), BitVecType(8))))
      i <- s.storeVar("stack", Scope.Global, BasilMapValue(Map.empty, MapType(BitVecType(64), BitVecType(8))))
      j <- s.storeVar("R31", Scope.Global, Scalar(SP))
      k <- s.storeVar("R29", Scope.Global, Scalar(FP))
      l <- s.storeVar("R30", Scope.Global, Scalar(LR))
      j <- s.storeVar("R31_in", Scope.Global, Scalar(SP))
      k <- s.storeVar("R29_in", Scope.Global, Scalar(FP))
      l <- s.storeVar("R30_in", Scope.Global, Scalar(LR))
      l <- s.storeVar("R0", Scope.Global, Scalar(BitVecLiteral(0, 64)))
      l <- s.storeVar("R1", Scope.Global, Scalar(BitVecLiteral(0, 64)))

      /** callee saved * */
      l <- s.storeVar("R19", Scope.Global, Scalar(BitVecLiteral(0, 64)))
      l <- s.storeVar("R20", Scope.Global, Scalar(BitVecLiteral(0, 64)))
      l <- s.storeVar("R21", Scope.Global, Scalar(BitVecLiteral(0, 64)))
      l <- s.storeVar("R22", Scope.Global, Scalar(BitVecLiteral(0, 64)))
      l <- s.storeVar("R23", Scope.Global, Scalar(BitVecLiteral(0, 64)))
      l <- s.storeVar("R24", Scope.Global, Scalar(BitVecLiteral(0, 64)))
      l <- s.storeVar("R25", Scope.Global, Scalar(BitVecLiteral(0, 64)))
      l <- s.storeVar("R26", Scope.Global, Scalar(BitVecLiteral(0, 64)))
      l <- s.storeVar("R27", Scope.Global, Scalar(BitVecLiteral(0, 64)))
      l <- s.storeVar("R28", Scope.Global, Scalar(BitVecLiteral(0, 64)))

      /** end callee saved * */
      _ <- s.storeVar("ghost-funtable", Scope.Global, BasilMapValue(Map.empty, MapType(BitVecType(64), BitVecType(64))))
      _ <- IntrinsicImpl.initFileGhostRegions(s)
    } yield (l)
  }

  def initialiseProgram[S, T <: Effects[S, InterpreterError]](f: T)(is: S, p: Program): S = {

    def initMemory(is: S, mem: String, mems: Iterable[MemorySection]): S = {
      var s = is
      for (memory <- mems.filter(m => m.address != 0 && m.bytes.size != 0)) {
        val bytes = memory.bytes.toList.map(Scalar(_))
        val addrs = memory.address until memory.address + bytes.size
        for (store <- addrs.zip(bytes)) {
          val (addr, value) = store
          s = State.execute(s, Eval.store(f)(mem, Scalar(BitVecLiteral(addr, 64)), List(value), Endian.BigEndian))
        }
      }
      s
    }

    var s = State.execute(is, initialState(f))

    for (proc <- p.procedures.filter(p => p.blocks.nonEmpty && p.address.isDefined)) {
      s = State.execute(
        s,
        Eval.storeSingle(f)(
          "ghost-funtable",
          Scalar(BitVecLiteral(proc.address.get, 64)),
          FunPointer(BitVecLiteral(proc.address.get, 64), proc.name, Run(IRWalk.firstInBlock(proc.entryBlock.get)))
        )
      )
    }

    val mainfun = p.mainProcedure
    s = State.execute(s, f.call("init_activation", Stopped(), Stopped()))
    s = initMemory(s, "mem", p.initialMemory.values)
    s = initMemory(s, "stack", p.initialMemory.values)
    s
  }

  /*
   * Calls a procedure that has a return, immediately evaluating the procedure
   *
   * Call a function, possibly dispatching to an intrinsic if the
   * procedure is not resolved, or resolves to a stub.
   */
  def callProcedure[S, T <: Effects[S, InterpreterError]](f: T)(
    targetProc: ProcSig | Procedure,
    params: Iterable[(LocalVar, Literal)]
  ): State[S, Map[LocalVar, Literal], InterpreterError] = {

    def intOf(n: String) = try {
      n.toInt
    } catch {
      case _ => n.hashCode()
    }

    val actualParams = params.toList.sortBy(p => intOf(p._1.name.stripPrefix("R").stripPrefix("V").stripSuffix("_in")))

    val target = targetProc match {
      case p: Procedure => Some(p)
      case _ => None
    }
    val proc = targetProc match {
      case p: ProcSig => p
      case p: Procedure => ProcSig(p.name, p.formalInParam.toList, p.formalOutParam.toList)
    }

    val call = for {
      // evaluate actual parms
      v <- {
        // perform call and push return stack frame and continuation

        val intrinsicName = target.map(_.procName).getOrElse(proc.name)
        if (LibcIntrinsic.intrinsicSigs.contains(intrinsicName)) {
          val intrinsic = LibcIntrinsic.intrinsicSigs(intrinsicName)
          // TODO if actual.isEmpty then load registes
          val actual = actualParams.toMap

          for {
            loadedParams: List[Option[BasilValue]] <-
              (if (actualParams.nonEmpty) {
                 // we were passed a list of parameters
                 State.mapM(
                   (p: LocalVar) =>
                     actual.get(p) match {
                       case Some(v) => State.pure[S, Option[BasilValue], InterpreterError](Some(Scalar(v)))
                       case None if !intrinsic.variadic =>
                         State.setError(Errored(s"Undefined intrinsic param $p in call $targetProc"))
                       case None => State.pure(None)
                     },
                   intrinsic.formalInParam
                 )
               } else {
                 // likely not in parameter form, load registers until we get an undefined one
                 for {
                   fs <- (State.mapM(
                     (p: LocalVar) =>
                       f.loadVar(p.name.stripSuffix("_in")).catchE {
                         case Left(l) if !intrinsic.variadic =>
                           State.setError(Errored(s"Undefined intrinsic param $p in call $targetProc ($l)"))
                         case Left(l) => State.pure(None)
                         case Right(r) => State.pure(Some(r))
                       },
                     intrinsic.formalInParam
                   ))
                 } yield (fs)
               })
            params: List[BasilValue] = loadedParams.takeWhile(_.isDefined).flatten
            returnval <- f.callIntrinsic(intrinsic.name, params)
            outparam = (returnval, proc.formalOutParam) match {
              case (_, Nil) => Map()
              case (Some(returnval), h :: Nil) => Map(h -> returnval)
              case (Some(returnval), h :: tl) =>
                Map(h -> returnval) ++ tl.map(t => t -> Scalar(BitVecLiteral(1234, size(t).get))).toMap
              case (None, rs) => rs.map(t => t -> Scalar(BitVecLiteral(1234, size(t).get))).toMap
            }
            stores = outparam.map(m => f.storeVar(m._1.name, m._1.toBoogie.scope, m._2))
            // store out params
            x <- State.sequence(State.pure(()), stores)
            x <- f.setNext(ReturnFrom(proc))
          } yield (())
        } else if (target.exists(_.entryBlock.isDefined)) {
          val block = target.get.entryBlock.get
          f.call(target.get.name, Run(IRWalk.firstInBlock(block)), ReturnFrom(proc))
        } else {
          State.setError(Errored(s"call to empty procedure: ${proc.name} / $target"))
        }
      }
      // set actual params in the callee state
      _ <- State.sequence(
        State.pure(()),
        actualParams.map(m => f.storeVar(m._1.name, m._1.toBoogie.scope, Scalar(m._2)))
      )
    } yield ()

    for {
      r <- call
      ret <- evalInterpreter(f, interpretContinuation(f))
      n <- f.getNext
      rv <-
        if (ret.isDefined) {
          ret.get match {
            case InterpretReturn.ReturnVal(v) => State.pure(v)
            case v => State.setError(Errored(s"Call didn't return value (got $v) $proc $n"))
          }
        } else {
          State.setError(Errored(s"Call to pure function should have returned a value, $proc"))
        }
    } yield (rv)
  }

  def initBSS[S, T <: Effects[S, InterpreterError]](f: T)(is: S, p: IRContext): S = {
    val bss = for {
      first <- p.symbols.find(s => s.name == "__bss_start__").map(_.value)
      last <- p.symbols.find(s => s.name == "__bss_end__").map(_.value)
      r <- (if (first == last) then None else Some((first, (last - first) * 8)))
      (addr, sz) = r
      st = {
        var s = is
        for (rgn <- Seq("mem", "stack")) {
          for (addr <- (first until last)) {
            s = State.execute(
              s,
              Eval.storeBV(f)(rgn, Scalar(BitVecLiteral(addr, 64)), BitVecLiteral(0, 8), Endian.LittleEndian)
            )
          }
        }
        s
      }

    } yield (st)

    bss match {
      case None => Logger.debug("Interpreter init: No BSS section initialised"); is
      case Some(init) => init
    }
  }

  def initProgState[S, T <: Effects[S, InterpreterError]](f: T)(p: IRContext, is: S): S = {
    var ist = initialiseProgram(f)(is, p.program)
    ist = initBSS(f)(ist, p)
    ist = State.execute(ist, InterpFuns.initRelocTable(f)(p))
    val st = State.putS(ist)
    val (fs, v) = st.f(is)
    v match {
      case Right(r) => fs
      case Left(e) => throw Exception(s"Init failed $e")
    }
  }

  def mainDefaultFunctionArguments(
    proc: Procedure,
    overlay: Map[String, BitVecLiteral] = Map()
  ): Map[LocalVar, Literal] = {
    val SP: BitVecLiteral = BitVecLiteral(0x78000000, 64)
    val FP: BitVecLiteral = SP
    val LR: BitVecLiteral = BitVecLiteral(BigInt("78000000", 16), 64)

    proc.formalInParam.toList.map {
      case l: LocalVar if overlay.contains(l.name) => l -> overlay(l.name)
      case l: LocalVar if l.name.startsWith("R0") => l -> BitVecLiteral(1, size(l).get)
      case l: LocalVar if l.name.startsWith("R31") => l -> SP
      case l: LocalVar if l.name.startsWith("R29") => l -> FP
      case l: LocalVar if l.name.startsWith("R30") => l -> LR
      case l: LocalVar => l -> BitVecLiteral(0, size(l).get)
    }.toMap
  }

  /* Intialise from ELF and Interpret program */
  def interpretEvalProg[S, T <: Effects[S, InterpreterError]](
    f: T
  )(p: IRContext, is: S): (S, Either[InterpreterError, Map[LocalVar, Literal]]) = {
    val begin = initProgState(f)(p, is)
    val main = p.program.mainProcedure
    callProcedure(f)(main, mainDefaultFunctionArguments(main)).f(begin)
  }

  def interpretEvalProgSkipInit[S, T <: Effects[S, InterpreterError]](
    f: T
  )(p: Program, begin: S): (S, Either[InterpreterError, Map[LocalVar, Literal]]) = {
    val main = p.mainProcedure
    callProcedure(f)(main, mainDefaultFunctionArguments(main)).f(begin)
  }

  /* Interpret IR program */
  def interpretEvalProg[S, T <: Effects[S, InterpreterError]](
    f: T
  )(p: Program, is: S): (S, Either[InterpreterError, Map[LocalVar, Literal]]) = {
    val begin = initialiseProgram(f)(is, p)
    val main = p.mainProcedure
    callProcedure(f)(main, mainDefaultFunctionArguments(main)).f(begin)
  }

  def interpretProg[S, T <: Effects[S, InterpreterError]](f: T)(p: IRContext, is: S): S = {
    interpretEvalProg(f)(p, is)._1
  }

  def interpretProg[S, T <: Effects[S, InterpreterError]](f: T)(p: Program, is: S): S = {
    interpretEvalProg(f)(p, is)._1
  }
}

def evalProc(p: Program, proc: Procedure, args: Map[LocalVar, Literal] = Map()): Map[LocalVar, Literal] = {
  val begin = InterpFuns.initialiseProgram(NormalInterpreter)(InterpreterState(), p)
  InterpFuns.callProcedure(NormalInterpreter)(proc, args).f(begin)._2 match {
    case Right(r) => r
    case Left(l) => throw Exception("interp error: " + l)
  }
}

def interpret(IRProgram: Program): InterpreterState = {
  InterpFuns.interpretProg(NormalInterpreter)(IRProgram, InterpreterState())
}

def interpret(IRProgram: IRContext): InterpreterState = {
  InterpFuns.interpretProg(NormalInterpreter)(IRProgram, InterpreterState())
}

def interpretEval(IRProgram: Program): (InterpreterState, Either[InterpreterError, Map[LocalVar, Literal]]) = {
  InterpFuns.interpretEvalProg(NormalInterpreter)(IRProgram, InterpreterState())
}

def interpretEval(IRProgram: IRContext): (InterpreterState, Either[InterpreterError, Map[LocalVar, Literal]]) = {
  InterpFuns.interpretEvalProg(NormalInterpreter)(IRProgram, InterpreterState())
}
