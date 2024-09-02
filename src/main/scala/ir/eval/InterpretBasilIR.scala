package ir.eval
import ir._
import ir.eval.BitVectorEval.*
import ir.*
import util.IRContext
import util.Logger
import util.functional.*
import util.functional.State.*
import boogie.Scope
import scala.collection.WithFilter

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.immutable
import scala.util.control.Breaks.{break, breakable}


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

  override def loadMemory(m: Memory, addr: Expr, endian: Endian, size: Int): State[S, Option[Literal], InterpreterError] = {
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
      res <- ir.eval.statePartialEvalExpr[S, InterpreterError](ldr)(e)
    } yield (res)
  }

  def evalBV[S, T <: Effects[S, InterpreterError]](f: T)(e: Expr): State[S, BitVecLiteral, InterpreterError] = {
    for {
      res <- evalExpr(f)(e)
      r <- State.pureE(res match {
      case l: BitVecLiteral => Right(l)
      case _                => Left(InterpreterError(Errored(s"Eval BV residual $e")))
    })
    } yield (r)
  }

  def evalInt[S, T <: Effects[S, InterpreterError]](f: T)(e: Expr): State[S, BigInt, InterpreterError] = {
    for {
      res <- evalExpr(f)(e)
      r <- State.pureE(res match {
      case l: IntLiteral => Right(l.value)
      case _                => Left(InterpreterError(Errored(s"Eval Int residual $e")))
    })
    } yield (r)
  }

  def evalBool[S, T <: Effects[S, InterpreterError]](f: T)(e: Expr): State[S, Boolean, InterpreterError] = {
    for {
      res <- evalExpr(f)(e)
      r <- State.pureE(res match {
      case l: BoolLit => Right(l == TrueLiteral)
      case _                => Left(InterpreterError(Errored(s"Eval Bool residual $e")))
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
      _ <- if (count == 0) then State.setError(InterpreterError(Errored(s"Attempted fractional load"))) else State.pure(())
      keys <- State.mapM(((i:Int) => State.pureE(BasilValue.unsafeAdd(addr, i))), (0 until count))
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
      case _ => State.setError(InterpreterError(Errored("Trued to load-concat non bv")))
    }
    (valsize, mapv) = x

    cells = size / valsize

    res <- load(f)(vname, addr, endian, cells) // actual load
    bvs: List[BitVecLiteral] <- (State.mapM ((c : BasilValue) => c match {
        case Scalar(bv @ BitVecLiteral(v, sz)) if sz == valsize => State.pure(bv)
        case c => State.setError(InterpreterError(TypeError(s"Loaded value of type ${c.irType} did not match expected type bv$valsize")))
      },res))
  } yield (bvs.foldLeft(BitVecLiteral(0, 0))((acc, r) => eval.evalBVBinExpr(BVCONCAT, acc, r)))

  def loadSingle[S, T <: Effects[S, InterpreterError]](f: T)(vname: String, addr: Scalar): State[S, BasilValue, InterpreterError] = {
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
      case m @ MapValue(_, MapType(kt, vt)) if kt == addr.irType && values.forall(v => v.irType == vt) => State.pure((m, kt, vt))
      case v => State.setError(InterpreterError(TypeError(s"Invalid map store operation to $vname : $v")))
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
    (mapval, vsize) = mem match {
      case m @ MapValue(_, MapType(kt, BitVecType(size))) if kt == addr.irType => (m, size)
      case v =>
        throw InterpreterError(
          TypeError(
            s"Invalid map store operation to $vname : ${v.irType} (expect [${addr.irType}] <- ${value.getType})"
          )
        )
    }
    cells = value.size / vsize
    _ = {
      if (cells < 1) {
        throw InterpreterError(MemoryError("Tried to execute fractional store"))
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

  def storeSingle[S, E, T <: Effects[S, E]](f: T)(vname: String, addr: BasilValue, value: BasilValue): State[S, Unit, E] = {
    f.storeMem(vname, Map((addr -> value)))
  }
}

case object InterpFuns {


  def initRelocTable[S, E, T <: Effects[S, E]](s: T)(p: Program, reladyn: Set[(BigInt, String)]): State[S, Unit, E] = {

    val data = reladyn.toList.flatMap(r => {
      val (offset, extfname) = r
      p.procedures.find(proc => proc.name == extfname).map(p => (offset, p)).toList
    })

    // TODO: will have to store 
    //  mem[rodata addr] = naddr
    //  ghost-funtable[naddr] = FunPointer - to intrinsic function
    // We could also dynamic link against something like musl, for things like string.h

    val fptrs = data.map((p) => {
      val (offset, proc) = p
      val addr = proc.address match {
            case Some(x) => x
            case None => println(s"No address for function ${proc.name} ${"%x".format(offset)}"); 0
      }
      // im guessing proc.address will be undefined and we will have to choose one for our intrinsic libc funcs
      (offset, FunPointer(BitVecLiteral(addr, 64), proc.name, Run(DirectCall(proc)))) 
    })

    val stores = fptrs.map((p) =>{
      val (offset, fptr) = p
      Eval.storeSingle[S,E,T](s)(
        "mem",
        Scalar(BitVecLiteral(offset, 64)),
        fptr
    )})

    for {
      _ <- State.sequence[S,Unit,E](State.pure(()), stores)
    } yield ()
  }

  /** Functions which compile BASIL IR down to the minimal interpreter effects.
    *
    * Each function takes as parameter an implementation of Effects[S]
    */

  def initialState[S, E, T <: Effects[S, E]](s: T): State[S, Unit, E] = {
    val SP: BitVecLiteral = BitVecLiteral(4096 - 16, 64)
    val FP: BitVecLiteral = BitVecLiteral(4096 - 16, 64)
    val LR: BitVecLiteral = BitVecLiteral(BigInt("FF", 16), 64)

    for {
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
    def initMemory(mem: String, mems: Iterable[MemorySection]) = {
      for {
        m <- State.sequence(
          State.pure(()),
          mems
            .filter(m => m.address != 0)
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
        State.pure(()),
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

  def interpretJump[S, T <: Effects[S, InterpreterError]](f: T)(j: Jump): State[S, Unit, InterpreterError] = {
    j match {
      case gt: GoTo if gt.targets.size == 1 => {
        f.setNext(Run(IRWalk.firstInBlock(gt.targets.head)))
      }
      case gt: GoTo =>
        val assumes = gt.targets.flatMap(_.statements.headOption).collect { case a: Assume =>
          a
        }
        if (assumes.size != gt.targets.size) {
          throw InterpreterError(Errored(s"Some goto target missing guard $gt"))
        }
        for {
          chosen: List[Assume] <- filterM((a: Assume) => Eval.evalBool(f)(a.body), assumes)

          res <- chosen match {
            case Nil      => f.setNext(Errored(s"No jump target satisfied $gt"))
            case h :: Nil => f.setNext(Run(h))
            case h :: tl  => f.setNext(Errored(s"More than one jump guard satisfied $gt"))
          }
        } yield (res)
      case r: Return      => f.doReturn()
      case h: Unreachable => f.setNext(EscapedControlFlow(h))
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
          n <-
            (if (!b) then {
               f.setNext(FailedAssertion(assert))
             } else {
               f.setNext(Run(s.successor))
             })
        } yield (n)
      case assume: Assume =>
        for {
          b <- Eval.evalBool(f)(assume.body)
          n <-
            (if (!b) {
               f.setNext(Errored(s"Assumption not satisfied: $assume"))
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
            } else {
              State.setError(InterpreterError(EscapedControlFlow(dc)))
              //f.setNext(Run(dc.successor))
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
              case none     => State.setError(InterpreterError(EscapedControlFlow(ic)))
            }
          } yield ()
        }
      }
      case _: NOP => f.setNext(Run(s.successor))
    }
  }

  def interpret[S, E, T <: Effects[S, E]](f: T, m: S): S = {
    State.evaluate(m, f.getNext) match {
      case Right(next) => {
        Logger.debug(s"eval $next")
        next match {
          case Run(c) =>  interpret(f, State.execute(m, f.interpretOne))
          case Stopped() => m
          case errorstop => m
        }
      }
      case Left(err) => m
    }
  }

  def interpretProg[S, T <: Effects[S, InterpreterError]](f: T)(p: Program, is: S): S = {
    val begin = State.execute(is, initialiseProgram(f)(p))
    interpret(f, begin)
  }


  def interpretProg[S, T <: Effects[S, InterpreterError]](f: T)(p: IRContext, is: S): S = {
    val st = for {
      _ <- initialiseProgram(f)(p.program)
      _ <- InterpFuns.initRelocTable(f)(p.program, p.externalFunctions.map(f => (f.offset, f.name)))
    } yield ()
    val begin = State.execute(is, st)
    interpret(f, begin)
  }
}

def interpret(IRProgram: Program): InterpreterState = {
  InterpFuns.interpretProg(NormalInterpreter)(IRProgram, InterpreterState())
}


def interpret(IRProgram: IRContext): InterpreterState = {
  InterpFuns.interpretProg(NormalInterpreter)(IRProgram, InterpreterState())
}

