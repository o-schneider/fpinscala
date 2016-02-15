package chapter2

object Exercise25 {

  def main(args: Array[String]): Unit = {
    val intToDouble: (Int) => Double = (a: Int) => a * 1.1
    val doubleToString: (Double) => String = (d: Double) => "val=" + d

    println("compose(doubleToString, intToDouble)(1), expected val=1.1, got " + compose(intToDouble, doubleToString)(1))
  }

  def compose[A, B, C]( f: A => B, g: B => C): A => C = (a: A) => g(f(a))
}
