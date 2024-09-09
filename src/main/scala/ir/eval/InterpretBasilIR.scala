package ir.eval
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

  def evalExpr[S, T <: Effects[S, InterpreterError]](f: T)(e: Expr): State[S, Expr, InterpreterError] = {
    val ldr = StVarLoader[S, T](f)
    for {
      res <- ir.eval.statePartialEvalExpr[S](ldr)(e)
    } yield (res)
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
      case mapv @ MapValue(_, MapType(_, BitVecType(sz))) => State.pure((sz, mapv))
      case _                                              => State.setError((Errored("Trued to load-concat non bv")))
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
              State.setError(
                TypeError(s"Loaded value of type ${c.irType} did not match expected type bv$valsize")
              )
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
  def store[S, T <: Effects[S, InterpreterError]](f: T)(
      vname: String,
      addr: BasilValue,
      values: List[BasilValue],
      endian: Endian
  ): State[S, Unit, InterpreterError] = for {
    mem <- f.loadVar(vname)
    x <- mem match {
      case m @ MapValue(_, MapType(kt, vt)) if kt == addr.irType && values.forall(v => v.irType == vt) =>
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
  def storeBV[S, T <: Effects[S, InterpreterError]](f: T)(
      vname: String,
      addr: BasilValue,
      value: BitVecLiteral,
      endian: Endian
  ): State[S, Unit, InterpreterError] = for {
    mem <- f.loadVar(vname)
    mr <- mem match {
      case m @ MapValue(_, MapType(kt, BitVecType(size))) if kt == addr.irType => State.pure((m, size))
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
}

class BASILInterpreter[S](f: Effects[S, InterpreterError]) extends Interpreter[S, InterpreterError](f) {

  def interpretOne: State[S, Boolean, InterpreterError] = {
    val next = for {
      next <- f.getNext
      _ <- State.pure(Logger.debug(s"$next"))
      r: Boolean <- (next match {
        case CallIntrinsic(tgt) => LibcIntrinsic.intrinsics(tgt)(f).map(_ => true)
        case Run(c: Statement)  => interpretStatement(f)(c).map(_ => true)
        case Run(c: Jump)       => interpretJump(f)(c).map(_ => true)
        case Stopped()          => State.pure(false)
        case ErrorStop(e)       => State.pure(false)
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
      case r: Return      => f.doReturn()
      case h: Unreachable => State.setError(EscapedControlFlow(h))
    }
  }

  def interpretStatement[S, T <: Effects[S, InterpreterError]](f: T)(s: Statement): State[S, Unit, InterpreterError] = {
    s match {
      case assign: Assign => {
        for {
          rhs <- Eval.evalBV(f)(assign.rhs)
          st <- f.storeVar(assign.lhs.name, assign.lhs.toBoogie.scope, Scalar(rhs))
          n <- f.setNext(Run(s.successor))
        } yield (st)
      }
      case assign: MemoryAssign =>
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
      case dc: DirectCall =>
        for {
          n <-
            if (dc.target.entryBlock.isDefined) {
              val block = dc.target.entryBlock.get
              f.call(dc.target.name, Run(block.statements.headOption.getOrElse(block.jump)), Run(dc.successor))
            } else if (LibcIntrinsic.intrinsics.contains(dc.target.name)) {
              f.call(dc.target.name, CallIntrinsic(dc.target.name), Run(dc.successor))
            } else {
              State.setError(EscapedControlFlow(dc))
            }
        } yield (n)
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

    val base = ctx.symbols.find(_.name == "__end__").get
    var addr = base.value
    var done = false
    var x = List[(String, FunPointer)]()

    def newAddr(): BigInt = {
      addr += 8
      addr
    }

    for ((fname, fun) <- LibcIntrinsic.intrinsics) {
      val name = fname.takeWhile(c => c != '@')
      x = (name, FunPointer(BitVecLiteral(newAddr(), 64), name, CallIntrinsic(name))) :: x
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
          >> (Eval.storeBV(s)(
            "mem",
            Scalar(BitVecLiteral(offset, 64)),
            fptr.addr,
            Endian.LittleEndian
          ))
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
      h <- s.storeVar("ghost-funtable", Scope.Global, MapValue(Map.empty, MapType(BitVecType(64), BitVecType(64))))
      h <- s.storeVar("mem", Scope.Global, MapValue(Map.empty, MapType(BitVecType(64), BitVecType(8))))
      i <- s.storeVar("stack", Scope.Global, MapValue(Map.empty, MapType(BitVecType(64), BitVecType(8))))
      j <- s.storeVar("R31", Scope.Global, Scalar(SP))
      k <- s.storeVar("R29", Scope.Global, Scalar(FP))
      l <- s.storeVar("R30", Scope.Global, Scalar(LR))
      l <- s.storeVar("R0", Scope.Global, Scalar(BitVecLiteral(0, 64)))
      l <- s.storeVar("R1", Scope.Global, Scalar(BitVecLiteral(0, 64)))
    } yield (l)
  }

  def initialiseProgram[S, T <: Effects[S, InterpreterError]](f: T)(p: Program): State[S, Unit, InterpreterError] = {
    def initMemory(mem: String, mems: ArrayBuffer[MemorySection]) = {
      for {
        m <- State.sequence(
          State.pure(()),
          mems
            .filter(m => m.address != 0 && m.bytes.size != 0)
            .map(memory =>
              Eval.store(f)(
                mem,
                Scalar(BitVecLiteral(memory.address, 64)),
                memory.bytes.toList.map(Scalar(_)),
                Endian.BigEndian
              )
            )
        )
      } yield ()
    }

    for {
      d <- initialState(f)
      funs <- State.sequence(
        State.pure(Logger.debug("INITIALISE FUNCTION ADDRESSES")),
        p.procedures
          .filter(p => p.blocks.nonEmpty && p.address.isDefined)
          .map((proc: Procedure) =>
            Eval.storeSingle(f)(
              "ghost-funtable",
              Scalar(BitVecLiteral(proc.address.get, 64)),
              FunPointer(BitVecLiteral(proc.address.get, 64), proc.name, Run(IRWalk.firstInBlock(proc.entryBlock.get)))
            )
          )
      )
      _ <- State.pure(Logger.debug("INITIALISE MEMORY SECTIONS"))
      mem <- initMemory("mem", p.initialMemory)
      mem <- initMemory("stack", p.initialMemory)
      mem <- initMemory("mem", p.readOnlyMemory)
      mem <- initMemory("stack", p.readOnlyMemory)
      mainfun = {
        p.mainProcedure
      }
      r <- f.call(mainfun.name, Run(IRWalk.firstInBlock(mainfun.entryBlock.get)), Stopped())
    } yield (r)
  }

  def initBSS[S, T <: Effects[S, InterpreterError]](f: T)(p: IRContext): State[S, Unit, InterpreterError] = {
    val bss = for {
      first <- p.symbols.find(s => s.name == "__bss_start__").map(_.value)
      last <- p.symbols.find(s => s.name == "__bss_end__").map(_.value)
      r <- (if (first == last) then None else Some((first, (last - first) * 8)))
      (addr, sz) = r
      st = {
        (rgn => Eval.storeBV(f)(rgn, Scalar(BitVecLiteral(addr, 64)), BitVecLiteral(0, sz.toInt), Endian.LittleEndian))
      }

    } yield (st)

    bss match {
      case None       => Logger.error("No BSS initialised"); State.pure(())
      case Some(init) => init("mem") >> init("stack")
    }
  }

  def initProgState[S, T <: Effects[S, InterpreterError]](f: T)(p: IRContext, is: S): S = {
    val st = (initialiseProgram(f)(p.program) >> initBSS(f)(p))
      >> InterpFuns.initRelocTable(f)(p)
    State.execute(is, st)
  }

  /* Intialise from ELF and Interpret program */
  def interpretProg[S, T <: Effects[S, InterpreterError]](f: T)(p: IRContext, is: S): S = {
    val begin = initProgState(f)(p, is)
    val interp = BASILInterpreter(f)
    interp.run(begin)
  }

  /* Interpret IR program */
  def interpretProg[S, T <: Effects[S, InterpreterError]](f: T)(p: Program, is: S): S = {
    val begin = State.execute(is, initialiseProgram(f)(p))
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
