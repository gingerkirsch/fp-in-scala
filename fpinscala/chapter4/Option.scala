sealed trait Option1[+A] {
  def map[B](f:A => B): Option1[B] = this match {
    case None1 => None1
    case Some1(v) => Some1(f(v))
  }

  def flatMap[B](f:A => Option1[B]): Option1[B] = this match {
    case None1 => None1
    case Some1(v) => f(v)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None1 => default
    case Some1(v) => v
  }

  def orElse[B >: A](ob: => Option1[B]): Option1[B] = this match {
    case None1 => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option1[A] = this match {
    case Some1(v) if (f(v)) => this
    case _ => None1
  }

  def mean(xs: Seq[Double]): Option1[Double] =
    if (xs.isEmpty) None1
    else Some1(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option1[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m,2))))
}

case class Some1[+A](get: A) extends Option1[A]
case object None1 extends Option1[Nothing]

