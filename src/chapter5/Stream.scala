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

  def foldRight[B](z: B)(f: (A, B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def takeWhile(p: A => Boolean): Stream[A] = foldRight[Stream[A]](Empty)((a, b) => if (p(a)) Stream.cons(a, b) else Empty)

  def headOption: Option[A] = foldRight[Option[A]](None)((a, opt) => Some(a))

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def map[B](f: A => B): Stream[B] = foldRight[Stream[B]](Empty)((a, s) => Stream.cons(f(a), s))

  def filter(p: A => Boolean): Stream[A] = foldRight[Stream[A]](Empty)((a, s) => if (p(a)) Stream.cons(a, s) else s)

  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight[Stream[B]](s)((a, s) => Stream.cons(a, s))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight[Stream[B]](Empty)((a, s) => f(a) append s)
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

}

object TestStream {
  def main(args: Array[String]) {
    println("Stream(1, 2, 3) toList : " + (Stream(1, 2, 3) toList))
    println("Stream(1, 2, 3) take 2 toList : " + (Stream(1, 2, 3) take 2 toList))
    println("Stream(1, 2, 3) drop 2 toList : " + (Stream(1, 2, 3) drop 2 toList))
    println("Stream(1, 2, 3).takeWhile0(x => x < 2) toList : " + (Stream(1, 2, 3).takeWhile0(x => x < 2) toList))
    println("Stream(1, 2, 3) exists (_ == 2) : " + (Stream(1, 2, 3) exists (_ == 2)))
    println("Stream(1, 2, 3) exists (_ == 5) : " + (Stream(1, 2, 3) exists (_ == 5)))
    println("Stream(1, 2, 3) forAll (_ % 2 == 0) : " + (Stream(1, 2, 3) forAll (_ % 2 == 0)))
    println("Stream(2, 4, 6) forAll (_ % 2 == 0) : " + (Stream(2, 4, 6) forAll (_ % 2 == 0)))
    println("Stream(1, 2, 3).takeWhile(x => x < 2) toList : " + (Stream(1, 2, 3).takeWhile(x => x < 2) toList))
    println("Stream(1, 2, 3) headOption0 : " + (Stream(1, 2, 3) headOption0))
    println("Stream(1, 2, 3) headOption : " + (Stream(1, 2, 3) headOption))
    println("Stream(1, 2, 3) map (_ * 2) toList : " + (Stream(1, 2, 3) map (_ * 2) toList))
    println("Stream(1, 2, 3) filter (_ % 2 == 0) toList : " + (Stream(1, 2, 3) filter (_ % 2 == 0) toList))
    println("Stream(1, 2, 3) append (Stream(4, 5, 6)) toList : " + (Stream(1, 2, 3) append Stream(4, 5, 6) toList))
    println("Stream(1, 2, 3) flatMap (a => Stream(a, a)) toList) : " + (Stream(1, 2, 3) flatMap (a => Stream(a, a)) toList))
  }
}