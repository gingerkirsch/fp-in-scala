object MyModule {
  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int = 0, acc: Int = 1): Int =
      if (n < 1) acc
      else go(n - 1, acc * n)
    go(n)
  }

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int = 0, a: Int = 0, b: Int = 1): Int =
      if (n == 0) a
      else go(n - 1, b, a + b)
    go(n)
  }

  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d"
    msg.format(n, factorial(n))
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)
    loop(0)
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (n >= as.length -1) true
      else if (ordered(as(n), as(n+1))) loop(n+1)
      else false
    loop(0)
  }

  def curry[A,B,C](f: (A,B) => C): A => B => C =
    (a: A) => (b: B) => f(a,b)

  def uncurry[A,B,C](f: A => B => C): (A,B) => C =
    (a: A, b: B) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatFactorial(7))
  }
}
