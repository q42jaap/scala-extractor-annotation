case class Yu(aap: String)

class Xxx {

  def body {
    val x: List[String] = Nil
    val y: Option[String] = None
    val z: String = ""

    x ++ y.toList ++ scala.collection.immutable.List(z)
  }
}

/*
Apply(
  Select(
    Apply(
      Select(
        Ident(newTermName("x")),
        newTermName("$plus$plus")
      ),
      List(
        Select(
          Ident(newTermName("y")),
          newTermName("toList")
        )
      )
    ),
    newTermName("$plus$plus")
  ),
  List(
    Apply(
      Ident(newTermName("List")),
      List(
        Ident(newTermName("z"))
      )
    )
  )
)
*/