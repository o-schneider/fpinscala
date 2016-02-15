package chapter4

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => b
    case Right(v) => Right(v)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(e) => Left(e)
      case Right(v) => f(v)
    }
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {a <- this; bb <- b} yield f(a, bb)
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as.foldRight[Either[E, List[B]]](Right(Nil))((el, acc) => f(el).map2(acc)(_ :: _))

  def sequence[E,A](es: List[Either[E,A]]): Either[E, List[A]] = traverse(es)((a) => a)
}

object TestEither {

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }

  def main(args: Array[String]) {
    val either = Right("1")
    println(either map (s => s.toInt))
    println(either orElse Left("boom"))
    println(Left("bam") orElse Left("boom"))
    println(either flatMap (s => Try(s.toInt)))
    println(Right("toto") flatMap (s => Try(s.toInt)))
    println(Right(1).map2(Right(2))(_+_))
    println(Either.traverse(List("1","2","3"))(s => Try(s.toInt)))
    println(Either.traverse(List("1","lala","3"))(s => Try(s.toInt)))
    println(Either.sequence(List(Right("1"),Right("2"),Right("3"))))
    println(Either.sequence(List(Right("1"),Left("TERRIBLE THINGS HAPPENED"),Right("3"))))

  }
}