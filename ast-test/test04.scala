class Xxx {
  def mkList[A](vals: List[_]): List[A] = ???

  def body {
    val vals = List(a, b, c)
    mkList[Int](vals)
  }
}