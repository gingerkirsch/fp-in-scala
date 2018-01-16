sealed trait Either1[+E,+A]{
  def map[B](f: A => B): Either1[E,B] = this match {
    case Right1(a) => Right1(f(a))
    case Left1(e) => Left1(e)
  }

  def flatMap[EE >: E,B](f: A => Either1[EE,B]): Either1[EE,B] = this match {
    case Right1(a) => f(a)
    case Left1(e) => Left1(e)
  }

  def orElse[EE >: E, B >: A](b: => Either1[EE,B]): Either1[EE,B] = this match {
    case Right1(a) => this
    case Left1(e) => b
  }

  def map2[EE >: E, B, C](b: Either1[EE,B])(f: (A,B) => C): Either1[EE,C] = (this,b) match {
    case (Right1(a), Right1(bb)) => Right1(f(a,bb))
    case (Left1(e),_) => Left1(e)
    case (_, Left1(e)) => Left1(e)
  }

  def sequence[E,A](as: List[Either1[E,A]]): Either1[E, List[A]] =
    as.foldRight[Either1[E, List[A]]](Right1(Nil))((a,b) => a.map2(b)(_ :: _ ))

  def traverse[E,A,B](as: List[A])(f: A => Either1[E,B]): Either1[E, List[B]] =
    as.foldRight[Either1[E, List[B]]](Right1(Nil))((a,b) => f(a).map2(b)(_ :: _))
 
}
case class Left1[+E](value: E) extends Either1[E, Nothing]
case class Right1[+A](value: A) extends Either1[Nothing, A]

object Either {
  def mean(xs: IndexedSeq[Double]): Either1[String, Double] =
    if (xs.isEmpty)
      Left1("mean of empty list!")
    else
      Right1(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either1[Exception, Int] =
    try Right1(x/y)
    catch { case e: Exception => Left1(e) }
}
