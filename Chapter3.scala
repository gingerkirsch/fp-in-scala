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
  def reverse[A](as: List[A]) = foldLeft(as, Nil: List[A])((x,y) => Cons(y,x))

  def foldLeftViaFoldRight[A,B](as: List[A], z: B)(f: (B,A) => B): B =
    foldRight(as, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(as),z)((x,y) => f(y,x))

  def appendViaFold[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1,a2)(Cons(_,_))
  def flatten[A](l: List[List[A]]): List[A] = foldRight(l, Nil:List[A])(appendViaFold(_,_))
  def add1(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((a,b) => Cons(a+1,b))
  def toString(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((a,b) => Cons(a.toString, b))
  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil: List[B])((a,b) => Cons(f(a),b))
  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])((a,b) => if (f(a)) Cons(a,b) else b)
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = foldRight(l, Nil: List[B])((a,b) => appendViaFold(f(a),b))
  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(a => if (f(a)) List(a) else Nil)

  def sumLists(a1: List[Int], a2: List[Int]): List[Int] = (a1,a2) match {
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, sumLists(xs, ys))
    case _ => Nil
  }

  def zipWith[A](a1: List[A], a2: List[A])(f: (A,A) => A): List[A] = (a1,a2) match {
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x,y), zipWith(xs,ys)(f))
    case _ => Nil
  }

  def take[A](l: List[A], n: Int): List[A] = (n,l) match {
    case (n, Nil) if n > 0  => throw new Exception("Not enough elements")
    case (0, _) => Nil
    case (n, _) => Cons(head(l), take(tail(l), n - 1))
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = lengthLeft(sub) match {
    case n if (n > lengthLeft(sup)) => false
    case _ => take(sup, length(sub)) == sub || hasSubsequence(tail(sup),sub)
  }
}
