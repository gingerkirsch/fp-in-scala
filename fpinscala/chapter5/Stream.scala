import Stream._

// different from List as takes explicit thunks instead of regular strict values
sealed trait Stream[+A] {
  /* Smart constructor takes care of memoizing the by-name arguments for the head and tail of the Cons.
  Thunk will only do its work once, when forced for the first time. Subsequent forces will return the cached lazyval */
  def cons[A](hd: => A, t1: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = t1
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h,t) => Some(h())  // explicit forcing of the thunk via h()
  }

  def toList: List[A] = this match {
      case Empty => Nil
      case Cons(h,t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h,t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h,_) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if (p(h())) => cons(h(), takeWhile(p))
    case _ => empty
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
