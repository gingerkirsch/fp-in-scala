trait RNG {
  def nextInt: (Int, RNG)
}

//type StateType[S, +A] = S => (A, S) // computation that carries the state along

// OR

case class State[S, +A](run: S => (A, S)) {
  import State._

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })

  def map[B](f: A => B): State[S,B] = flatMap(a => unit(f(a)))

  def map2[B,C](s: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => s.map(b => f(a, b)))
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S,A](a: A): State[S, A] = State(run = s => (a,s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = fs.foldRight(unit[S, List[A]](List.empty[A]))((sa, sb) => sa.map2(sb)(_ :: _))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S,S] = State(s => (s,s))
  def set[S](s: S): State[S,Unit] = State(_ => ((), s))

}
