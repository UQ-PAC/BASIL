package util.functional
import util.Logger
import sourcecode.Line, sourcecode.FileName
import java.io.*

val monlog = Logger.deriveLogger("statemonad")

/*
 * Flattened state monad with error.
 */
case class State[S, A, E](f: S => (S, Either[E, A])) {

  def unit[A](a: A): State[S, A, E] = State(s => (s, Right(a)))

  def >>(o: State[S, A, E]) = for {
    _ <- this
    x <- o
  } yield (x)

  def flatMap[B](
    f: A => State[S, B, E]
  )(implicit line: sourcecode.Line, file: sourcecode.FileName, name: sourcecode.Name): State[S, B, E] = State(s => {
    // monlog.debug(s"State.flatMap ${file.value}:${line.value}")
    val (s2, a) = this.f(s)
    val r = a match {
      case Left(l) => (s2, Left(l))
      case Right(a) => f(a).f(s2)
    }
    r
  })

  def map[B](f: A => B): State[S, B, E] = {
    State(s => {
      val (s2, a) = this.f(s)
      a match {
        case Left(l) => (s2, Left(l))
        case Right(a) => (s2, Right(f(a)))
      }
    })
  }

  def flatMapE(f: E => State[S, A, E]): State[S, A, E] = {
    State(s => {
      val (s2, a) = this.f(s)
      a match {
        case Left(l) => f(l).f(s2)
        case Right(_) => (s2, a)
      }
    })
  }
}

object State {
  def get[S, A, E](f: S => A): State[S, A, E] = State(s => (s, Right(f(s))))
  def getE[S, A, E](f: S => Either[E, A]): State[S, A, E] = State(s => (s, f(s)))
  def getS[S, E]: State[S, S, E] = State((s: S) => (s, Right(s)))
  def putS[S, E](s: S): State[S, Unit, E] = State((_) => (s, Right(())))
  def modify[S, E](f: S => S): State[S, Unit, E] = State(s => (f(s), Right(())))
  def modifyE[S, E](f: S => Either[E, S]): State[S, Unit, E] = State(s =>
    f(s) match {
      case Right(ns) => (ns, Right(()))
      case Left(e) => (s, Left(e))
    }
  )
  def execute[S, A, E](s: S, c: State[S, A, E])(implicit
    line: sourcecode.Line,
    file: sourcecode.FileName,
    name: sourcecode.Name
  ): S = {
    // Logger.debug(s"state.execute")(line, file, name)
    c.f(s) match {
      case (s, Left(r)) => {
        monlog.info(s"ERROR: $r")
        s
      }
      case (s, _) => s
    }
  }
  def evaluate[S, A, E](s: S, c: State[S, A, E]): Either[E, A] = c.f(s)._2

  def setError[S, A, E](e: E): State[S, A, E] = State(s => (s, Left(e)))

  def pure[S, A, E](a: A): State[S, A, E] = State((s: S) => (s, Right(a)))
  def pureE[S, A, E](
    a: Either[E, A]
  )(implicit line: sourcecode.Line, file: sourcecode.FileName, name: sourcecode.Name): State[S, A, E] = {
    a match {
      case Left(l) => monlog.info(s"pureE error $l")(line, file, name)
      case _ => ()
    }
    State((s: S) => (s, a))
  }

  def sequence[S, V, E](ident: State[S, V, E], xs: Iterable[State[S, V, E]]): State[S, V, E] = {
    xs.foldRight(ident)((l, r) =>
      for {
        x <- l
        y <- r
      } yield (y)
    )
  }

  def filterM[A, S, E](m: (A => State[S, Boolean, E]), xs: Iterable[A]): State[S, List[A], E] = {
    xs.foldRight(pure(List[A]()))((b, acc) => acc.flatMap(c => m(b).map(v => if v then b :: c else c)))
  }

  def mapM[A, B, S, E](m: (A => State[S, B, E]), xs: Iterable[A])(implicit
    line: sourcecode.Line,
    file: sourcecode.FileName,
    name: sourcecode.Name
  ): State[S, List[B], E] = {
    // monlog.debug(s"State.mapM (${xs.size} items) ${file.value}:${line.value}")
    xs.foldRight(pure(List[B]()))((b, acc) => acc.flatMap(c => m(b).map(v => v :: c)))
  }

  def protect[S, V, E](f: () => State[S, V, E], fnly: PartialFunction[Exception, E]): State[S, V, E] = {
    State((s: S) =>
      try {
        f().f(s)
      } catch {
        case e: Exception if fnly.isDefinedAt(e) => (s, Left(fnly(e)))
      }
    )
  }

  def protectPure[S, V, E](f: () => V, fnly: PartialFunction[Exception, E]): State[S, V, E] = {
    State((s: S) =>
      try {
        (s, Right(f()))
      } catch {
        case e: Exception if fnly.isDefinedAt(e) => (s, Left(fnly(e)))
      }
    )
  }

}

def protect[T](x: () => T, fnly: PartialFunction[Exception, T]): T = {
  try {
    x()
  } catch {
    case e: Exception if fnly.isDefinedAt(e) => fnly(e)
  }
}
