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

  def map2[A,B,C](a: Option1[A], b: Option1[B])(f: (A,B) => C): Option1[C] =
    (a,b) match {
      case (Some1(a), Some1(b)) => Some1(f(a,b))
      case (_,_) => None1
    }

  def sequence[A](a: List[Option1[A]]): Option1[List[A]] =
    a.foldRight[Option1[List[A]]](Some1(Nil))((a,b) => map2(a,b)(_ :: _))

  def traverse[A,B](a: List[A])(f: A => Option1[B]): Option1[List[B]] =
    a.foldRight[Option1[List[B]]](Some1(Nil))((a,b) => map2(f(a),b)(_ :: _))

  def map2_1[A,B,C](a: Option1[A], b: Option1[B])(f: (A,B) => C): Option1[C] =
    a flatMap (aa => b map (bb => f(aa,bb)))

  def map2_2[A,B,C](a: Option1[A], b: Option1[B])(f: (A,B) => C): Option1[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

}

case class Some1[+A](get: A) extends Option1[A]
case object None1 extends Option1[Nothing]

