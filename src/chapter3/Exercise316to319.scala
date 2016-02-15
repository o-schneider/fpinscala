package chapter3

object Exercise316to319 {

  def main(args: Array[String]): Unit = {
    println(List.add1(List(1, 2)))
    println(List.doubleToString(List(1d, 2d, 0d)))
    println(List.map(List(1d, 2d, 0d))(_ * 2))
    println(List.filter(List(1d, 2d, 0d))(_ % 2 == 0))

    println(List.flatMap(List(1, 2, 3))(i => List(i, i)))
    println(List.filterFromFlatMap(List(1d, 2d, 8d))(_ % 2 == 0))

  }


}
