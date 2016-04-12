package chapter7

import java.util.concurrent.{Callable, Executors}

object Par {
  type Par[A] = () => A

  def unit[A](a: A): Par[A] = () => a

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = unit(f(run(a), run(b)))

  def fork[A](a: => Par[A]): Par[A] = a

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](a: Par[A]): A = {
    Executors.newSingleThreadExecutor().submit(new Callable[A] {
      override def call(): A = {
        val a1: A = a()
        println("called for " + a1)
        a1
      }
    }).get()
  }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.length <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }

  def main(args: Array[String]) {
    println(run(sum(IndexedSeq(1, 2, 3))))
  }
}