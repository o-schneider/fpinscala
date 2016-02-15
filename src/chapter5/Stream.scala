package chapter5

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
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

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => this
    case Cons(h, t) => if (p(h())) Stream.cons(h(), t() takeWhile p) else Empty
  }

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
    val stream = Stream(1, 2, 3)
    println(stream toList)
    println(stream take 2 toList)
    println(stream drop 2 toList)
    println(stream.takeWhile(x => x < 2) toList)
  }
}