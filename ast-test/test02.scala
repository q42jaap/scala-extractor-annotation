import scala.annotation.StaticAnnotation

class extract(val name: String) extends StaticAnnotation
class special(val name: String = "specialxxx") extends extract(name)

case class Xxx(@special aap: String, @extract("nootje") noot:String)