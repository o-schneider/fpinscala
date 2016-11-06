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
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    lazy val range: Rand[Int] = RNG.int.flatMap(i => {
      val mod = i % stopExclusive
      if (i + (stopExclusive - 1) - mod >= start) State.unit(mod) else range
    })
    Gen(range)
  }

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(RNG.int.map(i => i > 0))

  def listofN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

}

object Main {
  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(42)
    val gen = Gen.choose(-1, 100)
    val run1 = gen.sample.run(rng)
    val run2 = gen.sample.run(run1._2)
    println(run1._1)
    println(run2._1)
  }
}
