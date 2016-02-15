package chapter3

object Exercise35 {

  def main(args: Array[String]) {
    println("" + List.dropWhile(List(2, 2, 3, 4, 5), (i: Int) => {
      val inter: Int = i % 2
      println(inter)
      (inter == 0)
    }))
    println("" + List.dropWhile(List(1, 2, 3, 4, 5), (i: Int) => i < 10))
  }


}
