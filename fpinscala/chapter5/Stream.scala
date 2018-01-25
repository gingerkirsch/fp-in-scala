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

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def existsViaFoldRight(p: A => Boolean): Boolean = foldRight(false)((a,b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a,b) => p(a) && b)

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = foldRight(empty[A])((a,b) => if (p(a)) cons(a,b) else empty)

  def headOptionViaFoldRight: Option[A] = foldRight(None: Option[A])((a,b) => Some(a))

  def map[B](p: A => B): Stream[B] = foldRight(empty[B])((a,b) => cons(p(a),b))

  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((a,b) => if (p(a)) cons(a,b) else b)

  def append[B>:A](s: => Stream[B]): Stream[B] = foldRight(s)((a,b) => cons(a,b))

  def flatMap[B](p: A => Stream[B]): Stream[B] = foldRight(empty[B])((a,b) => p(a) append b)

  def find(p: A => Boolean): Option[A] = filter(p).headOption

  def constant[A](a: A): Stream[A] = cons(a, constant(a))
  def infiniteOfIntegers(n: Int): Stream[Int] = cons(n, infiniteOfIntegers(n + 1))
  def infiniteFibs(a: Int = 0, b: Int = 1): Stream[Int] = cons(a, infiniteFibs(b, a + b))

  def unfold[A, S](z: S)(f: S => Option[(A,S)]): Stream[A] = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
