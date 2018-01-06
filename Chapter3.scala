sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x: Int = List[Int](1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new Exception("Cannot get a tail from list of one element")
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => xs
  }

  def setHead[A](l: List[A], y: A): List[A] = l match {
    case Nil => throw new Exception("Cannot replace first element of an empty list")
    case Cons(x, xs) => Cons(y, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = (n,l) match {
    case (n, Nil) if n > 0  => throw new Exception("Not enough elements")
    case (0, _) => l
    case (n, _) => drop(tail(l), n - 1)
  }

  def head[A](l: List[A]): A = l match {
    case Nil => throw new Exception("Cannot take head of empty list")
    case Cons(x, xs) => x
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new Exception("Cannot init an empty list or list of one element")
    case Cons(y, Cons(x, Nil)) => Cons(y, Nil)
    case Cons(x, xs) => Cons(x,init(xs))
  }

  def dropWhileCurried[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Cons(x, xs) if f(x) => dropWhileCurried(xs)(f)
    case _ => as
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  def product2haltOn0 = ??? //impossible

  val l1 = foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))

  def length[A](as: List[A]): Int = foldRight(as, 0)((_,n) => n + 1)

  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs,f(z,x))(f)
  }

  def sumLeft(ns: List[Int]) = foldLeft(ns, 0)(_ + _)
  def productLeft(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)
  def lengthLeft[A](as: List[A]): Int = foldLeft(as, 0)((n, _) => n + 1)

}
