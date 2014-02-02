package util.annotatedextractorpoc

import util.objmapper.ObjMapper
import scala.annotation.StaticAnnotation


//class special extends extract("strings") with StaticAnnotation

class ancestorIds extends ExtractAnnotation
//class title extends ExtractAnnotation

case class MainClass(@ancestorIds foo: String, @ancestorIds foo2: Option[String], bar: String, baz: Int)

object AncestorIdsExtractor extends ExtractorBase[ancestorIds] {
  object implicits {
    implicit val extMainClass = Macros.extractor[MainClass, String, ancestorIds]
  }
}

object ExtractorExample extends App {

  def extractorExample1() {

    import AncestorIdsExtractor.implicits._

    val mainVal = MainClass(foo = "fooVal", foo2 = Some("foo2Val"), bar = "barVal", baz = 42)
    val mappedVal: List[String] = AncestorIdsExtractor.getValues(mainVal)

    println("Example1")
    printValues(mainVal, mappedVal)
  }
  /*
    def extractorExample2() {
      import Macros._

      implicit val extractor = annotatedExtractor[MainClass, SubSetClass2]

      val mainVal = MainClass(foo = "fooVal", bar = "barVal", baz = 42)
      val mappedVal: SubSetClass2 = ObjMapper.mapValue(mainVal)

      println("Example2")
      printValues(mainVal, mappedVal)
    }

    def extractorExample3() {
      import Macros._

      implicit val extractor = annotatedExtractor[MainClass, SubSetClass3]

      val mainVal = MainClass(foo = "fooVal", bar = "barVal", baz = 42)
      val mappedVal: SubSetClass3 = ObjMapper.mapValue(mainVal)

      println("Example3")
      printValues(mainVal, mappedVal)
    }

    def extractorNotCompilesExample() {
      // enable line below to see compile error
      // val extractor = Macros.objMapper[MainClass, NotASubSetClass]
    }
  */

  def printValues(mainVal: Any, mappedVal: Any) {
    println()
    println(s"main value: $mainVal")
    println(s"mapped value: $mappedVal")
    println()
    println()
  }

  extractorExample1()
  //extractorExample2()
  //extractorExample3()


}
