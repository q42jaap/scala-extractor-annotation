package util.annotatedextractorpoc

import util.objmapper.ObjMapper

object ExtractorExample extends App {

  case class MainClass(@extract("strings")foo: String, @extract("strings")bar: String, baz: Int)

  case class SubSetClass1(bar: String)

  case class SubSetClass2(foo: String, bar: String)

  case class SubSetClass3(bar: String, baz: Int)

  case class Extractor1( strings: List[String])

  def extractorExample1() {
    import Macros._

    implicit val extractor = annotatedExtractor[MainClass, SubSetClass1]

    val mainVal = MainClass(foo = "fooVal", bar = "barVal", baz = 42)
    val mappedVal: SubSetClass1 = ObjMapper.mapValue(mainVal)

    println("Example1")
    printValues(mainVal, mappedVal)
  }

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


  def printValues(mainVal: Any, mappedVal: Any) {
    println()
    println(s"main value: $mainVal")
    println(s"mapped value: $mappedVal")
    println()
    println()
  }

  extractorExample1()
  extractorExample2()
  extractorExample3()


}
