package util.functional

case class State[S, +A](f: S => (S, A)) {

  def unit[A](a: A): State[S, A] = State(s => (s, a))


  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    // println(s"flatmap ${this.f} $f")
    val (s2, a) = this.f(s)
    f(a).f(s2)
  })


  def map[B](f: A => B): State[S, B] = {
    State(s => {
      val (s2, a) = this.f(s)
      (s2, f(a))
    })
  }
}


object State {
  def get[S,A](f: S => A) : State[S, A] = State(s => (s, f(s)))
  def getS[S] : State[S,S] = State((s:S) => (s,s))
  def putS[S](s: S) : State[S,_] = State((_) => (s,()))
  def modify[S](f: S => S) : State[S, Unit] = State(s => (f(s), ()))
  def execute[S, A](s: S, c: State[S,A]) : S = c.f(s)._1
  def evaluate[S, A](s: S, c: State[S,A])  : A = c.f(s)._2

  def pure[S, A](a: A) : State[S, A] = State((s:S) => (s, a))

  def sequence[S, V](ident: State[S,V], xs: Iterable[State[S,V]]) : State[S,V] = {
    xs.foldRight(ident)((l,r) => for {
      x <- l
      y <- r
    } yield(y)) 
  }

  def sequence[V](xs: Iterable[Option[V]]) : Option[V] = {
    xs.reduceRight((a, b) => a match {
      case Some(x) => Some(x)
      case None =>  b
    }) 
  }

  def filterM[A, S](m : (A => State[S, Boolean]), xs: Iterable[A]): State[S, List[A]] = {
    xs.foldRight(pure(List[A]()))((b,acc) => acc.flatMap(c => m(b).map(v => if v then b::c else c)))
  }

  def mapM[A, B, S](m : (A => State[S, B]), xs: Iterable[A]): State[S, List[B]] = {
    xs.foldRight(pure(List[B]()))((b,acc) => acc.flatMap(c => m(b).map(v => v::c)))
  }


}

def protect[T](x: () => T, fnly: PartialFunction[Exception, T]): T = {
  try {
    x()
  } catch {
    case e: Exception if fnly.isDefinedAt(e) => fnly(e)
  }
}
