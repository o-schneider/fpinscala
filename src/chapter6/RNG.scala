package chapter6

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

case class State[S, +A](run: S => (A, S)) {

  def flatMap[B](g: A => State[S, B]): State[S, B] = State(s => {
    val (i, s2) = run(s)
    g(i).run(s2)
  })

  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

  def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => rb.map(b => f(a, b)))


}

object State {
  def unit[A, S](a: A): State[S, A] = new State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = fs.foldRight[State[S, List[A]]](State.unit(List()))((ra, acc) => ra.map2(acc)(_ :: _))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

object RNG {

//  def double(rng: RNG): (Double, RNG) = {
//    val (i, r) = nonNegativeInt(rng)
//    (i / (Int.MaxValue.toDouble + 1), r)
//  }
//
//  def intDouble(rng: RNG): ((Int, Double), RNG) = {
//    val (i, r) = rng.nextInt
//    val (i2, r2) = double(r)
//    ((i, i2), r2)
//  }
//
//  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
//    val (i, r) = intDouble(rng)
//    ((i._2, i._1), r)
//  }
//
//
//  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
//    @tailrec
//    def go(acc: List[Int], count: Int, rng: RNG): (List[Int], RNG) = {
//      if (count > 0) {
//        val (i, r) = rng.nextInt
//        go(i :: acc, count - 1, r)
//      } else
//        (acc, rng)
//    }
//    go(Nil, count, rng)
//  }

  type Rand[+A] = State[RNG, A]

  def nonNegativeInt: Rand[Int] = State(rng => {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  })

  val int: Rand[Int] = State(_.nextInt)

  val nonNegativeEven: Rand[Int] = nonNegativeInt.map(i => i - i % 2)

  val double: Rand[Double] = nonNegativeInt.map(_ / (Int.MaxValue.toDouble + 1))

  val double3: Rand[(Double, Double, Double)] = double.map2(double)((d1, d2) => (d1,d2)).map2(double)((t, d3) => (t._1, t._2, d3))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = State(rng => {
    val (a, rng2) = ra.run(rng)
    val (b, rng3) = rb.run(rng2)
    (f(a, b), rng3)
  })

  def boolean: Rand[Boolean] = State(rng => rng.nextInt match { case (i,rng2) => (i%2==0,rng2) })

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = ra.map2(rb)((_, _))

  val intDouble: Rand[(Int, Double)] = both(int, double)

  val doubleInt: Rand[(Double, Int)] = both(double, int)

  def ints(count: Int): Rand[List[Int]] = State.sequence(List.fill(count)(int))

  def nonNegativeLessThan(n: Int): Rand[Int] = nonNegativeInt.flatMap(i => {
    val mod = i % n
    if (i + (n - 1) - mod >= 0) State.unit(mod) else nonNegativeLessThan(n)
  })

  val rollDie: Rand[Int] = nonNegativeLessThan(6).map(_ + 1)
}

object TestRNG {
  def main(args: Array[String]) {
    val rng = new SimpleRNG(42)
    println("RNG.double(rng) = " + RNG.double.run(rng)._1)
    println("RNG.intDouble(rng) = " + RNG.intDouble.run(rng)._1)
    println("RNG.doubleInt(rng) = " + RNG.doubleInt.run(rng)._1)
    println("RNG.double3(rng) = " + RNG.double3.run(rng)._1)
    println("RNG.ints(3)(rng) = " + RNG.ints(3).run(rng))
    println("RNG.intsWithSequence(3)(rng) = " + RNG.ints(3).run(rng))
    println("RNG.ints(0)(rng) = " + RNG.ints(0).run(rng))
    println("RNG.intsWithSequence(0)(rng) = " + RNG.ints(0).run(rng))
    println("RNG.ints(1)(rng) = " + RNG.ints(1).run(rng))
    println("RNG.intsWithSequence(1)(rng) = " + RNG.ints(1).run(rng))
    println("RNG.doubleWithMap = " + RNG.double.run(rng)._1)
    println("RNG.randIntDouble = " + RNG.intDouble.run(rng))
    println("RNG.nonNegativeLessThan = " + RNG.nonNegativeLessThan(4).run(rng))
    println("RNG.nonNegativeEven = " + RNG.nonNegativeEven.run(rng))

    println("RNG.mapWithFlatMap(RNG.int)(a => a + \"\")(rng) = " + RNG.int.map(a => a + "").run(rng))
    println("RNG.map2WithFlatMap(RNG.int, RNG.int)((a, b) => a + \" \" + b)(rng) = " + RNG.int.map2(RNG.int)((a, b) => a + " " + b).run(rng))

    println("RNG.rollDie(new SimpleRNG(5))._1 = " + RNG.rollDie.run(new SimpleRNG(5))._1)
  }
}
