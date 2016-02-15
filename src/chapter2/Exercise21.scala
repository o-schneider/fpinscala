package chapter2

object Exercise21 {

  def main(args: Array[String]) {
    println(" fib(10) expected 55, got " +  fib(10))
    println(" fib(12) expected 144, got " +  fib(12))
  }

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n1: Int, n2: Int, stepLeft: Int): Int = {
      if (stepLeft == 0) return n2
      go(n2, n1 + n2, stepLeft - 1)
    }
    go(0, 1, n - 1)
  }
}
