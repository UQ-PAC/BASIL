package ir.eval
import ir.transforms.Substitute
import ir._
import ir.eval.BitVectorEval.*
import ir.*
import util.IRContext
import util.Logger
import util.functional.*
import util.functional.State.*
import boogie.Scope
import collection.mutable.ArrayBuffer

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.immutable
import scala.util.control.Breaks.{break, breakable}
import translating.ELFSymbol

/** Abstraction for memload and variable lookup used by the expression evaluator.
  */
case class StVarLoader[S, F <: Effects[S, InterpreterError]](f: F) extends Loader[S, InterpreterError] {

  def getVariable(v: Variable): State[S, Option[Literal], InterpreterError] = {
    for {
      v <- f.loadVar(v.name)
    } yield ((v match {
      case Scalar(l) => Some(l)
      case _         => None
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
                case _         => None
              }
            )
        case l: Literal => Eval.loadBV(f)(m.name, Scalar(l), endian, size).map(Some(_))
        case _          => get((s: S) => None)
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
        case _          => Left((Errored(s"Eval BV residual $e")))
      })
    } yield (r)
  }

  def evalBV[S, T <: Effects[S, InterpreterError]](f: T)(e: Expr): State[S, BitVecLiteral, InterpreterError] = {
    for {
      res <- evalExpr(f)(e)
      r <- State.pureE(res match {
        case l: BitVecLiteral => Right(l)
        case _                => Left((Errored(s"Eval BV residual $e")))
      })
    } yield (r)
  }

  def evalInt[S, T <: Effects[S, InterpreterError]](f: T)(e: Expr): State[S, BigInt, InterpreterError] = {
    for {
      res <- evalExpr(f)(e)
      r <- State.pureE(res match {
        case l: IntLiteral => Right(l.value)
        case _             => Left((Errored(s"Eval Int residual $e")))
      })
    } yield (r)
  }

  def evalBool[S, T <: Effects[S, InterpreterError]](f: T)(e: Expr): State[S, Boolean, InterpreterError] = {
    for {
      res <- evalExpr(f)(e)
      r <- State.pureE(res match {
        case l: BoolLit => Right(l == TrueLiteral)
        case _          => Left((Errored(s"Eval Bool residual $e")))
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
        case Endian.BigEndian    => values
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
        case Endian.BigEndian    => values
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
        case Endian.BigEndian    => extractVals.reverse.map(Scalar(_))
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
        case _                        => State.setError(Errored(s"Not pointer : $src"))
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

class BASILInterpreter[S](f: Effects[S, InterpreterError]) extends Interpreter[S, InterpreterError](f) {

  def interpretOne: State[S, Boolean, InterpreterError] = {
    val next = for {
      next <- f.getNext
      _ <- State.pure(Logger.debug(s"$next"))
      r: Boolean <- (next match {
        case Intrinsic(tgt)    => LibcIntrinsic.intrinsics(tgt)(f).map(_ => true)
        case Run(c: Statement) => interpretStatement(f)(c).map(_ => true)
        case ReturnTo(c)       => interpretReturn(f)(c).map(_ => true)
        case Run(c: Jump)      => interpretJump(f)(c).map(_ => true)
        case Stopped()         => State.pure(false)
        case ErrorStop(e)      => State.pure(false)
      })
    } yield (r)

    next.flatMapE((e: InterpreterError) => {
      f.setNext(ErrorStop(e)).map(_ => false)
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
            case Nil      => State.setError(Errored(s"No jump target satisfied $gt"))
            case h :: Nil => f.setNext(Run(h))
            case h :: tl  => State.setError(Errored(s"More than one jump guard satisfied $gt"))
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

  def interpretReturn[S, T <: Effects[S, InterpreterError]](f: T)(s: DirectCall): State[S, Unit, InterpreterError] = {
    for {
      outs <- State.mapM(
        ((bindout: (LocalVar, Variable)) => {
          for {
            rhs <- Eval.evalLiteral(f)(bindout._1)
          } yield (bindout._2, rhs)
        }),
        s.outParams
      )
      c <- State.sequence(State.pure(()), outs.map(m => f.storeVar(m._1.name, m._1.toBoogie.scope, Scalar(m._2))))
      _ <- f.setNext(Run(s.successor))
    } yield (c)
  }

  def interpretStatement[S, T <: Effects[S, InterpreterError]](f: T)(s: Statement): State[S, Unit, InterpreterError] = {
    s match {
      case assign: LocalAssign => {
        for {
          rhs <- Eval.evalLiteral(f)(assign.rhs)
          st <- f.storeVar(assign.lhs.name, assign.lhs.toBoogie.scope, Scalar(rhs))
          n <- f.setNext(Run(s.successor))
        } yield (st)
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
          actualParams <- State.mapM(
            (p: (LocalVar, Expr)) =>
              for {
                v <- Eval.evalLiteral(f)(p._2)
              } yield (p._1, v),
            dc.actualParams
          )
          _ <- {
            if (LibcIntrinsic.intrinsics.contains(dc.target.procName)) {
              f.call(dc.target.name, Intrinsic(dc.target.procName), ReturnTo(dc))
            } else if (dc.target.entryBlock.isDefined) {
              val block = dc.target.entryBlock.get
              f.call(dc.target.name, Run(block.statements.headOption.getOrElse(block.jump)), ReturnTo(dc))
            } else {
              State.setError(EscapedControlFlow(dc))
            }
          }
          _ <- State.sequence(
            State.pure(()),
            actualParams.map(m => f.storeVar(m._1.name, m._1.toBoogie.scope, Scalar(m._2)))
          )
        } yield ()
      }
      case ic: IndirectCall => {
        if (ic.target == Register("R30", 64)) {
          f.doReturn()
        } else {
          for {
            addr <- Eval.evalBV(f)(ic.target)
            fp <- f.evalAddrToProc(addr.value.toInt)
            _ <- fp match {
              case Some(fp) => f.call(fp.name, fp.call, Run(ic.successor))
              case none     => State.setError(EscapedControlFlow(ic))
            }
          } yield ()
        }
      }
      case _: NOP => f.setNext(Run(s.successor))
    }
  }
}

object InterpFuns {

  def initRelocTable[S, T <: Effects[S, InterpreterError]](s: T)(ctx: IRContext): State[S, Unit, InterpreterError] = {

    val p = ctx.program

    var addr = ctx.symbols.find(_.name == "__end__").map(_.value).getOrElse(BigInt(123456))
    var done = false
    var x = List[(String, FunPointer)]()

    def newAddr(): BigInt = {
      addr += 8
      addr
    }

    for ((fname, fun) <- LibcIntrinsic.intrinsics) {
      val name = fname.takeWhile(c => c != '@')
      x = (name, FunPointer(BitVecLiteral(newAddr(), 64), name, Intrinsic(name))) :: x
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
    s = State.execute(s, f.call(mainfun.name, Run(IRWalk.firstInBlock(mainfun.entryBlock.get)), Stopped()))
    // l <- State.sequence(State.pure(()), mainfun.formalInParam.toList.map(i => f.storeVar(i.name, i.toBoogie.scope, Scalar(BitVecLiteral(0, size(i).get)))))
    s
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
      case None       => Logger.error("No BSS initialised"); is
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
      case Left(e)  => throw Exception(s"Init failed $e")
    }
  }

  /* Intialise from ELF and Interpret program */
  def interpretProg[S, T <: Effects[S, InterpreterError]](f: T)(p: IRContext, is: S): S = {
    val begin = initProgState(f)(p, is)
    val interp = BASILInterpreter(f)
    interp.run(begin)
  }

  /* Interpret IR program */
  def interpretProg[S, T <: Effects[S, InterpreterError]](f: T)(p: Program, is: S): S = {
    val begin = initialiseProgram(f)(is, p)
    val interp = BASILInterpreter(f)
    interp.run(begin)
  }
}

def interpret(IRProgram: Program): InterpreterState = {
  InterpFuns.interpretProg(NormalInterpreter)(IRProgram, InterpreterState())
}

def interpret(IRProgram: IRContext): InterpreterState = {
  InterpFuns.interpretProg(NormalInterpreter)(IRProgram, InterpreterState())
}
