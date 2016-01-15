object Exercise22 {

  def main(args: Array[String]) {
    println("isSorted(Array(1,2,3),(a1:Int,a2:Int) => a1 < a2) returns true, got " + isSorted(Array(1, 2, 3), (a1: Int, a2: Int) => a1 < a2))
    println("isSorted(Array(1,2,3,2),(a1:Int,a2:Int) => a1 < a2) returns false, got " + isSorted(Array(1, 2, 3, 2), (a1: Int, a2: Int) => a1 < a2))
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    if (as.length <= 1) return true
    def go(i: Int): Boolean = {
      if (i >= as.length) true
      else if (ordered(as(i - 1), as(i))) go (i + 1)
      else false
    }
    go(1)
  }
}
