object Exercise313 {

  def main(args: Array[String]): Unit = {
    println(List.foldLeftFromFoldRight(List(1,2,3),0)(_+_))
  }


}
