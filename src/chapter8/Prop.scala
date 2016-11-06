package chapter8

import java.awt.image.SampleModel

import chapter6.RNG.Rand
import chapter6.{RNG, SimpleRNG, State}
import chapter8.Prop.{FailedCase, SuccessCount}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
}

trait Prop {
  def check(): Either[(FailedCase, SuccessCount), SuccessCount]

  //def &&(p: Prop): Prop = () => check() && p.check()
}

case class Gen[A](sample: Rand[A])

object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(RNG.nonNegativeInt.map(n => start + n % (stopExclusive - start)))

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(RNG.boolean)

  def listofN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

}

object Main {
  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(42)
    val gen = Gen.choose(-100, 100)
    val run1 = gen.sample.run(rng)
    println(run1._1)
  }
}
