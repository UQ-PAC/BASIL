package ir.eval
import ir._
import ir.eval.BitVectorEval.*
import ir.*
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
case class StVarLoader[S, E, F <: Effects[S, E]](f: F) extends Loader[S, E] {

  def getVariable(v: Variable): State[S, Option[Literal], E] = {
    for {
      v <- f.loadVar(v.name)
    } yield ((v match {
      case Scalar(l) => Some(l)
      case _         => None
    }))
  }

  override def loadMemory(m: Memory, addr: Expr, endian: Endian, size: Int): State[S, Option[Literal], E] = {
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

  def evalExpr[S, E, T <: Effects[S, E]](f: T)(e: Expr): State[S, Expr, E] = {
    val ldr = StVarLoader[S, E, T](f)
    for {
      res <- ir.eval.statePartialEvalExpr[S, E](ldr)(e)
    } yield (res)
  }

  def evalBV[S, E, T <: Effects[S, E]](f: T)(e: Expr): State[S, BitVecLiteral, E] = {
    val ldr = StVarLoader[S, E, T](f)
    for {
      res <- ir.eval.statePartialEvalExpr[S, E](ldr)(e)
    } yield (res match {
      case l: BitVecLiteral => l
      case _                => throw InterpreterError(Errored(s"Eval BV residual $e"))
    })
  }

  def evalInt[S, E, T <: Effects[S, E]](f: T)(e: Expr): State[S, BigInt, E] = {
    val ldr = StVarLoader[S, E, T](f)
    for {
      res <- ir.eval.statePartialEvalExpr[S, E](ldr)(e)
    } yield (res match {
      case l: IntLiteral => l.value
      case _             => throw InterpreterError(Errored(s"Eval BV residual $e"))
    })
  }

  def evalBool[S, E, T <: Effects[S, E]](f: T)(e: Expr): State[S, Boolean, E] = {
    val ldr = StVarLoader[S, E, T](f)
    for {
      res <- ir.eval.statePartialEvalExpr[S, E](ldr)(e)
    } yield (res match {
      case l: BoolLit => l == TrueLiteral
      case _          => throw InterpreterError(Errored(s"Eval BV residual $e"))
    })
  }

  /*--------------------------------------------------------------------------------*/
  /* Load functions                                                                 */
  /*--------------------------------------------------------------------------------*/

  def load[S, E, T <: Effects[S, E]](
      f: T
  )(vname: String, addr: Scalar, endian: Endian, count: Int): State[S, List[BasilValue], E] = {
    if (count == 0) {
      throw InterpreterError(Errored(s"Attempted fractional load"))
    }
    val keys = (0 until count).map(i => BasilValue.unsafeAdd(addr, i))
    for {
      values <- f.loadMem(vname, keys.toList)
      vals = endian match {
        case Endian.LittleEndian => values.reverse
        case Endian.BigEndian    => values
      }
    } yield (vals.toList)
  }

  /** Load and concat bitvectors */
  def loadBV[S, E, T <: Effects[S, E]](
      f: T
  )(vname: String, addr: Scalar, endian: Endian, size: Int): State[S, BitVecLiteral, E] = for {
    mem <- f.loadVar(vname)
    (valsize, mapv) = mem match {
      case mapv @ MapValue(_, MapType(_, BitVecType(sz))) => (sz, mapv)
      case _ => throw InterpreterError(Errored("Trued to load-concat non bv"))
    }

    cells = size / valsize

    res <- load(f)(vname, addr, endian, cells) // actual load
    bvs: List[BitVecLiteral] = {
      val rr = res.map {
        case Scalar(bv @ BitVecLiteral(v, sz)) if sz == valsize => bv
        case c =>
          throw InterpreterError(TypeError(s"Loaded value of type ${c.irType} did not match expected type bv$valsize"))
      }
      rr
    }
  } yield (bvs.foldLeft(BitVecLiteral(0, 0))((acc, r) => eval.evalBVBinExpr(BVCONCAT, acc, r)))

  def loadSingle[S, E, T <: Effects[S, E]](f: T)(vname: String, addr: Scalar): State[S, BasilValue, E] = {
    for {
      m <- load(f)(vname, addr, Endian.LittleEndian, 1)
    } yield (m.head)
  }

  /*--------------------------------------------------------------------------------*/
  /* Store functions                                                                */
  /*--------------------------------------------------------------------------------*/

  /* Expand addr for number of values to store */
  def store[S, E, T <: Effects[S, E]](f: T)(
      vname: String,
      addr: BasilValue,
      values: List[BasilValue],
      endian: Endian
  ): State[S, Unit, E] = for {
    mem <- f.loadVar(vname)
    (mapval, keytype, valtype) = mem match {
      case m @ MapValue(_, MapType(kt, vt)) if kt == addr.irType && values.forall(v => v.irType == vt) => (m, kt, vt)
      case v => throw InterpreterError(TypeError(s"Invalid map store operation to $vname : $v"))
    }
    keys = (0 until values.size).map(i => BasilValue.unsafeAdd(addr, i))
    vals = endian match {
      case Endian.LittleEndian => values.reverse
      case Endian.BigEndian    => values
    }
    x <- f.storeMem(vname, keys.zip(vals).toMap)
  } yield (x)

  /** Extract bitvec to bytes and store bytes */
  def storeBV[S, E, T <: Effects[S, E]](f: T)(
      vname: String,
      addr: BasilValue,
      value: BitVecLiteral,
      endian: Endian
  ): State[S, Unit, E] = for {
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

    keys = (0 until cells).map(i => BasilValue.unsafeAdd(addr, i))
    s <- f.storeMem(vname, keys.zip(vs).toMap)
  } yield (s)

  def storeSingle[S, E, T <: Effects[S, E]](f: T)(vname: String, addr: BasilValue, value: BasilValue): State[S, Unit, E] = {
    f.storeMem(vname, Map((addr -> value)))
  }
}

case object InterpFuns {

  /** Functions which compile BASIL IR down to the minimal interpreter effects.
    *
    * Each function takes as parameter an implementation of Effects[S]
    */

  def initialState[S, E, T <: Effects[S, E]](s: T): State[S, Unit, E] = {
    val SP: BitVecLiteral = BitVecLiteral(4096 - 16, 64)
    val FP: BitVecLiteral = BitVecLiteral(4096 - 16, 64)
    val LR: BitVecLiteral = BitVecLiteral(BigInt("FF", 16), 64)

    for {
      h <- s.storeVar("funtable", Scope.Global, MapValue(Map.empty, MapType(BitVecType(64), BitVecType(64))))
      h <- s.storeVar("mem", Scope.Global, MapValue(Map.empty, MapType(BitVecType(64), BitVecType(8))))
      i <- s.storeVar("stack", Scope.Global, MapValue(Map.empty, MapType(BitVecType(64), BitVecType(8))))
      j <- s.storeVar("R31", Scope.Global, Scalar(SP))
      k <- s.storeVar("R29", Scope.Global, Scalar(FP))
      l <- s.storeVar("R30", Scope.Global, Scalar(LR))
    } yield (l)
  }

  def initialiseProgram[S, E, T <: Effects[S, E]](f: T)(p: Program): State[S, Unit, E] = {
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
                Endian.LittleEndian
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
              "funtable",
              Scalar(BitVecLiteral(proc.address.get, 64)),
              FunPointer(BitVecLiteral(proc.address.get, 64), proc.name, Run(IRWalk.firstInBlock(proc.entryBlock.get)))
            )
          )
      )
      mem <- initMemory("mem", p.initialMemory)
      mem <- initMemory("stack", p.initialMemory)
      mem <- initMemory("mem", p.readOnlyMemory)
      mem <- initMemory("stack", p.readOnlyMemory)
      r <- f.call(p.mainProcedure.name, Run(IRWalk.firstInBlock(p.mainProcedure.entryBlock.get)), Stopped())
    } yield (r)
  }

  def interpretJump[S, E, T <: Effects[S, E]](f: T)(j: Jump): State[S, Unit, E] = {
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

  def interpretStatement[S, E, T <: Effects[S, E]](f: T)(s: Statement): State[S, Unit, E] = {
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
              f.setNext(Run(dc.successor))
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
              case none     => f.setNext(EscapedControlFlow(ic))
            }
          } yield ()
        }
      }
      case _: NOP => f.setNext(Run(s.successor))
    }
  }

  def interpret[S, E, T <: Effects[S, E]](f: T, m: S): S = {
    val next = State.evaluate(m, f.getNext)
    Logger.debug(s"eval $next")
    next match {
      case Run(c) =>  interpret(f, State.execute(m, f.interpretOne))
      case Stopped() => m
      case errorstop => m
    }
  }

  def interpretProg[S, E, T <: Effects[S, E]](f: T)(p: Program, is: S): S = {
    val begin = State.execute(is, initialiseProgram(f)(p))
    // State.execute[S,Unit](is, )
    interpret(f, begin)
  }
}

def interpret(IRProgram: Program): InterpreterState = {
  InterpFuns.interpretProg(NormalInterpreter)(IRProgram, InterpreterState())
}

