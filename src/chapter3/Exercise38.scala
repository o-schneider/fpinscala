package chapter3

object Exercise38 {

  def main(args: Array[String]) {
    println(List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))
  }


}
