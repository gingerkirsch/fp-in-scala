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
    case Cons(h, t) => Some(h()) // explicit forcing of the thunk via h()
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if (p(h())) => cons(h(), takeWhile(p))
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

  def existsViaFoldRight(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

  def headOptionViaFoldRight: Option[A] = foldRight(None: Option[A])((a, b) => Some(a))

  def map[B](p: A => B): Stream[B] = foldRight(empty[B])((a, b) => cons(p(a), b))

  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((a, b) => cons(a, b))

  def flatMap[B](p: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, b) => p(a) append b)

  def find(p: A => Boolean): Option[A] = filter(p).headOption

  def ones: Stream[Int] = cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def infiniteOfIntegers(n: Int): Stream[Int] = cons(n, infiniteOfIntegers(n + 1))

  def infiniteFibs(a: Int = 0, b: Int = 1): Stream[Int] = cons(a, infiniteFibs(b, a + b))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty
  }

  def onesUnfold: Stream[Int] = unfold(1)(_ => Some(1, 1))

  def constantUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))

  def fromUnfold(n: Int): Stream[Int] = unfold(n)(x => Some(x, x + 1))

  def fibsUnfold: Stream[Int] = unfold((0, 1)) { case (a, b) => Some(a, (b, a + b)) }

  def mapUnfold[B](p: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some(p(h()), t())
    case _ => None
  }

  def takeUnfold(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(h, t), n) if n > 1 => Some(h(), (t(), n - 1))
    case (Cons(h, t), n) if n == 1 => Some(h(), (empty, 0))
    case _ => None
  }

  def takeWhileUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case (Cons(h, t)) if (p(h())) => Some(h(), t())
    case _ => None
  }

  def zipWithUnfold[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    case _ => None
  }

  /*  def zipAllUnfold[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some()
    case (Cons(h1, t1), Empty) => Some()
    case (Empty, Cons(h2, t2) => Some()
    case (Empty, Empty) => None
  }*/

  def hasSubsequence[A](s: Stream[A]): Boolean = ???

  def startsWith[A](s: Stream[A]): Boolean = ???

  def tails: Stream[Stream[A]] = ???

  def hasSubsequenceTails[A](s: Stream[A]): Boolean = tails exists (_ startsWith s)

  def scanRight = ???

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
