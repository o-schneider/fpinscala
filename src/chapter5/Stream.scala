package chapter5


sealed trait Stream[+A] {
  def headOption0: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => this
    case Cons(h, t) => if (n <= 0) Empty else Stream.cons(h(), t().take(n - 1))
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => this
    case Cons(h, t) => if (n <= 0) this else t() drop (n - 1)
  }

  def takeWhile0(p: A => Boolean): Stream[A] = this match {
    case Empty => this
    case Cons(h, t) => if (p(h())) Stream.cons(h(), t() takeWhile0 p) else Empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def takeWhile(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((a, b) => if (p(a)) Stream.cons(a, b) else Empty)

  def headOption: Option[A] = foldRight[Option[A]](None)((a, opt) => Some(a))

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def map[B](f: A => B): Stream[B] = foldRight(Stream.empty[B])((a, s) => Stream.cons(f(a), s))

  def filter(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((a, s) => if (p(a)) Stream.cons(a, s) else s)

  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((a, s) => Stream.cons(a, s))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Stream.empty[B])((a, s) => f(a) append s)

  def find(p: A => Boolean): Option[A] = filter(p) headOption

  def takeUnfold(n: Int): Stream[A] = Stream.unfold(this, n) {
    case (Cons(h, t), x) if x > 0 => Some(h(), (t(), x - 1))
    case _ => None
  }

  def mapUnfold[B](f: A => B): Stream[B] = Stream.unfold(this) {
    case Cons(h, t) => Some(f(h()), t())
    case _ => None
  }

  def takeWhileUnfold(p: A => Boolean): Stream[A] = Stream.unfold(this) {
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }

  def zipWith[B, C](b: Stream[B])(f: (A, B) => C): Stream[C] = Stream.unfold(this, b) {
    case (Cons(ha, ta), Cons(hb, tb)) => Some(f(ha(), hb()), (ta(), tb()))
    case _ => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = Stream.unfold(this, s2) {
    case (Cons(ha, ta), Empty) => Some((Some(ha()), None), (ta(), Stream.empty))
    case (Empty, Cons(hb, tb)) => Some((None, Some(hb())), (Stream.empty, tb()))
    case (Cons(ha, ta), Cons(hb, tb)) => Some((Some(ha()), Some(hb())), (ta(), tb()))
    case _ => None
  }

  def startsWith[B >: A](s: Stream[B]): Boolean = zipAll(s) takeWhile (_._2.isDefined) forAll (t => (for {a <- t._1; b <- t._2} yield a == b).getOrElse(false))

  def tails: Stream[Stream[A]] = Stream.unfold(this) {
    case Cons(h, t) => Some(Stream.cons(h(), t()), t())
    case _ => None
  } append Stream(Stream.empty)

  def hasSubsequence[B >: A](s: Stream[B]): Boolean = tails exists (_ startsWith s)

  def scanRight[B >: A](z: B)(f: (A, B) => B): Stream[B] = foldRight(Stream(z))((v, acc) => acc match {
    case Cons(h, t) => Stream.cons(f(v, h()), acc)
    case _ => Empty
  })
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  val ones0: Stream[Int] = cons(1, ones0)

  def constant0[A](a: A): Stream[A] = cons(a, constant0(a))

  def from0(n: Int): Stream[Int] = cons(n, from0(n + 1))

  def fibs0: Stream[Int] = fibs0()

  private def fibs0(x: Int = 0, y: Int = 1): Stream[Int] = cons(x, fibs0(y, x + y))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((x, y)) => cons(x, unfold(y)(f))
    case _ => empty
  }

  val ones: Stream[Int] = constant(1)

  def constant[A](a: A): Stream[A] = unfold(a)(a => Some(a, a))

  def from(n: Int): Stream[Int] = unfold(n)(n => Some(n + 1, n + 1))

  def fibs: Stream[Int] = unfold(0, 1)(n => Some(n._1, (n._2, n._1 + n._2)))

}

object TestStream {
  def main(args: Array[String]) {
    println("Stream(1, 2, 3) toList : " + (Stream(1, 2, 3) toList))
    println("Stream(1, 2, 3) take 2 toList : " + (Stream(1, 2, 3) take 2 toList))
    println("Stream(1, 2, 3) drop 2 toList : " + (Stream(1, 2, 3) drop 2 toList))
    println("Stream(1, 2, 3) takeWhile0 (x => x < 2) toList : " + (Stream(1, 2, 3) takeWhile0 (x => x < 2) toList))
    println("Stream(1, 2, 3) exists (_ == 2) : " + (Stream(1, 2, 3) exists (_ == 2)))
    println("Stream(1, 2, 3) exists (_ == 5) : " + (Stream(1, 2, 3) exists (_ == 5)))
    println("Stream(1, 2, 3) forAll (_ % 2 == 0) : " + (Stream(1, 2, 3) forAll (_ % 2 == 0)))
    println("Stream(2, 4, 6) forAll (_ % 2 == 0) : " + (Stream(2, 4, 6) forAll (_ % 2 == 0)))
    println("Stream(1, 2, 3) takeWhile (x => x < 2) toList : " + (Stream(1, 2, 3) takeWhile (x => x < 2) toList))
    println("Stream(1, 2, 3) headOption0 : " + (Stream(1, 2, 3) headOption0))
    println("Stream(1, 2, 3) headOption : " + (Stream(1, 2, 3) headOption))
    println("Stream(1, 2, 3) map (_ * 2) toList : " + (Stream(1, 2, 3) map (_ * 2) toList))
    println("Stream(1, 2, 3) filter (_ % 2 == 0) toList : " + (Stream(1, 2, 3) filter (_ % 2 == 0) toList))
    println("Stream(1, 2, 3) append (Stream(4, 5, 6)) toList : " + (Stream(1, 2, 3) append Stream(4, 5, 6) toList))
    println("Stream(1, 2, 3) flatMap (a => Stream(a, a)) toList) : " + (Stream(1, 2, 3) flatMap (a => Stream(a, a)) toList))
    println("=== EXECUTION TRACE")
    println(Stream(1, 2, 3, 4).map(a => {
      println("map on " + a)
      a + 10
    }).filter(a => {
      println("filter on " + a)
      a % 2 == 0
    }).toList)
    println("=== END")
    println("Stream ones0 take 5 toList : " + (Stream.ones0 take 5 toList))
    println("Stream constant0 5 take 5 toList : " + (Stream.constant0(5) take 5 toList))
    println("Stream.from0(3) take 5 toList : " + (Stream from0 3 take 5 toList))
    println("Stream.fibs0 take 7 toList : " + (Stream.fibs0 take 7 toList))
    println("Stream.unfold(1)(s => Option(s * s, s + 1)) take 5 toList); should be (1²;2²;3²;4²... etc) : " + (Stream.unfold(1)(s => Option(s * s, s + 1)) take 5 toList))
    println("Stream ones take 5 toList : " + (Stream.ones take 5 toList))
    println("Stream constant 5 take 5 toList : " + (Stream.constant(5) take 5 toList))
    println("Stream.from(3) take 5 toList : " + (Stream.from(3) take 5 toList))
    println("Stream.fibs take 7 toList : " + (Stream.fibs take 7 toList))
    println("Stream(1, 2, 3) takeUnfold 1 toList : " + (Stream(1, 2, 3) takeUnfold 1 toList))
    println("Stream(1, 2, 3) mapUnfold (_ * 2) toList : " + (Stream(1, 2, 3) mapUnfold (_ * 2) toList))
    println("Stream(1, 2, 3) takeWhileUnfold (x => x < 2) toList : " + (Stream(1, 2, 3) takeWhileUnfold (x => x < 2) toList))
    println("Stream(1, 2, 3).zipWith(Stream(4, 5, 6))(_ * _) toList : " + (Stream(1, 2, 3).zipWith(Stream(4, 5, 6))(_ * _) toList))
    println("Stream(1, 2, 3).zipWith(Stream(4, 5))(_ * _) toList : " + (Stream(1, 2, 3).zipWith(Stream(4, 5))(_ * _) toList))
    println("Stream(2, 3).zipWith(Stream(4, 5, 6))(_ * _) toList : " + (Stream(2, 3).zipWith(Stream(4, 5, 6))(_ * _) toList))
    println("Stream(1, 2, 3) zipAll Stream(4, 5, 6) toList : " + (Stream(1, 2, 3) zipAll Stream(4, 5, 6) toList))
    println("Stream(2, 3) zipAll Stream(4, 5, 6) toList : " + (Stream(2, 3) zipAll Stream(4, 5, 6) toList))
    println("Stream(1, 2, 3) zipAll Stream(4, 5) toList : " + (Stream(1, 2, 3) zipAll Stream(4, 5) toList))
    println("Stream(1, 2, 3) startsWith  Stream(1, 2) : " + (Stream(1, 2, 3) startsWith Stream(1, 2)))
    println("Stream(1, 2, 3) startsWith  Stream(2, 3) : " + (Stream(1, 2, 3) startsWith Stream(2, 3)))
    println("Stream(1, 2, 3) startsWith Stream.empty : " + (Stream(1, 2, 3) startsWith Stream.empty))
    println("Stream.empty startsWith Stream(1, 2, 3) : " + (Stream.empty startsWith Stream(1, 2, 3)))
    println("Stream(1, 2, 3).tails.map(_ toList) toList : " + (Stream(1, 2, 3).tails.map(_ toList) toList))
    println("Stream(1, 2, 3) hasSubsequence  Stream(2, 3) : " + (Stream(1, 2, 3) hasSubsequence Stream(2, 3)))
    println("Stream(1, 2, 3) hasSubsequence  Stream(1, 2) : " + (Stream(1, 2, 3) hasSubsequence Stream(1, 2)))
    println("Stream(1, 2, 3) hasSubsequence Stream.empty) : " + (Stream(1, 2, 3) hasSubsequence Stream.empty))
    println("Stream.empty hasSubsequence Stream(1, 2)) : " + (Stream.empty hasSubsequence Stream(1, 2)))
    println("Stream(1,2,3).scanRight(0)(_+_) toList : " + (Stream(1, 2, 3).scanRight(0)(_ + _) toList))
  }
}