sealed trait List[+A]

// `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing]

// A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A] {

}

object List {


  // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match {
    // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def head[A](list: List[A]): A = list match {
    case Nil => sys.error("head on empty list")
    case Cons(h, t) => h
  }

  def tail[A](list: List[A]): List[A] = list match {
    case Nil => sys.error("tail on empty list")
    case Cons(h, t) => t
  }

  def setHead[A](h: A, list: List[A]): List[A] = list match {
    case Nil => sys.error("called on empty list")
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (n == 0) l else drop(t, n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h: A, t) => if (f(h)) dropWhile(t, f) else l
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def init[A](l: List[A]): List[A] = {
    def go(l: List[A], acc: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(h, Nil) => acc
      case Cons(h, t) => go(t, append(acc, List(h)))
    }
    go(l, Nil)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }


  def sum2(ns: List[Int]) = foldRight(ns, 0)((x, y) => x + y)

  def sum3(ns: List[Int]) = foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Int]) = foldRight(ns, 1.0)(_ * _)

  def product3(ns: List[Int]) = foldLeft(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int = foldRight(as, 0)((a, acc) => acc + 1)

  def length2[A](as: List[A]): Int = foldLeft(as, 0)((acc, a) => acc + 1)

  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, Nil: List[A])((acc, l) => {

      Cons(l, acc)
    })
  }

  def reverse2[A](as: List[A]): List[A] = {
    foldRight(as, Nil: List[A])((l, acc) => Cons(l, acc))
  }

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def foldLeftFromFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(as, (b: B) => b)((ax: A, fx: (B) => B) => (b: B) => fx(f(b, ax)))(z)
  }

  def appendFromFoldRight[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _))

  def concat[A](a: List[List[A]]): List[A] = foldLeft(a, Nil: List[A])((acc, ax) => List.append(acc, ax))

  def add1(list: List[Int]): List[Int] = foldRight(list, Nil: List[Int])((value, acc) => Cons(value + 1, acc))

  def doubleToString(list: List[Double]): List[String] = foldRight(list, Nil: List[String])((value, acc) => Cons(value.toString, acc))

  def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil: List[B])((value, acc) => Cons(f(value), acc))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil: List[A])((value, acc) => if (f(value)) Cons(value, acc) else acc)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = List.concat(List.map(as)(f))

  def filterFromFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if (f(a)) List(a) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addPairwise(t1,t2))
  }

}
