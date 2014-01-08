class Xxx {
  def mkList[A](vals: List[_]): List[A] = ???

  def body {
    val vals = List()
    mkList[Int](vals)
  }
}