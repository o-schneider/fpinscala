package chapter3

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r);
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](t: Tree[A], f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l, f), map(r, f))
  }

  def fold[A, B](t: Tree[A])(z: A => B)(f: (B, B) => B): B = t match {
    case Leaf(v) => z(v)
    case Branch(l, r) => f(fold(l)(z)(f): B, fold(r)(z)(f): B)
  }

  def sizeFold[A](t: Tree[A]): Int = fold(t)(x => 1)(1 + _ + _)

  def maximumFold(t: Tree[Int]): Int = fold(t)(x => x)(_ max _)

  def depthFold[A](t: Tree[A]): Int = fold(t)(x => 1)((a, b) => 1 + (a max b))

  def mapFold[A, B](t: Tree[A], f: A => B): Tree[B] = fold(t)(x => Leaf(f(x)): Tree[B])(Branch(_, _))
}

object Exercises {

  def main(args: Array[String]): Unit = {
    val tree =
      Branch(
        Branch(
          Branch(
            Leaf(1),
            Branch(
              Leaf(99),
              Leaf(4))),
          Leaf(19)),
        Leaf(20)
      )

    println(Tree.size(tree))
    println(Tree.sizeFold(tree))
    println(Tree.maximum(tree))
    println(Tree.maximumFold(tree))
    println(Tree.depth(tree))
    println(Tree.depthFold(tree))
    val f: (Int) => String = x => "crazy" + x
    println(Tree.map(tree, f))
    println(Tree.mapFold(tree, f))
  }

}