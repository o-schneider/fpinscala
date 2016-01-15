object Exercise23_24 {

  def main(args: Array[String]): Unit = {
    val fInDoToString = (i: Int, d: Double) => i + " " + d
    println(fInDoToString(1, 1.1))
    val curriedF: (Int) => (Double) => String = curry(fInDoToString)
    println(curriedF(1)(1.1))
    val curriedF1: (Double) => String = curriedF(1)
    println(curriedF1(1.1))
    println(uncurry(curriedF)(2, 2.2))
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => {
    (b: B) => {
      f(a, b)
    }
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))
}
