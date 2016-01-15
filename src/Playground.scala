object Playground {

  class Animal
  class Cat extends Animal
  def main(args: Array[String]): Unit = {
    val as:List[Animal] = List[Animal]()
    val bs:List[Cat] = List[Cat]()

    println (bs.isInstanceOf[List[Animal]])
  }

}
