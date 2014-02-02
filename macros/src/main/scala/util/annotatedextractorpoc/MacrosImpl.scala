package util.annotatedextractorpoc

import language.experimental.macros

import reflect.macros.Context
import util.objmapper.ObjMapper
import scala.annotation.StaticAnnotation

object MacrosImpl {

  private def mkHelper[TSource: c.WeakTypeTag, T: c.WeakTypeTag, TAnn <: ExtractAnnotation: c.WeakTypeTag]
  (c: Context): Helper[c.type, TSource, T, TAnn] = new Helper[c.type, TSource, T, TAnn](c) {
  }

  def extractorImpl[TSource: c.WeakTypeTag, T: c.WeakTypeTag, TAnn <: ExtractAnnotation: c.WeakTypeTag]
  (c: Context)(): c.Expr[Extractor[TSource, T, TAnn]] = {
    import c.universe._
    val helper = mkHelper[TSource, T, TAnn](c)

    val body = helper.getValuesBody

    reify {
      new Extractor[TSource, T, TAnn] {
        def getValues(obj: TSource): List[T] = body.splice
      }
    }
  }
}

private abstract class Helper[C <: Context, TSource: C#WeakTypeTag, T: C#WeakTypeTag, TAnn <: ExtractAnnotation: C#WeakTypeTag](val c: C) {

  import c.universe._

  val annType = c.weakTypeOf[TAnn]
  val sourceType = c.weakTypeOf[TSource]
  val tType = c.weakTypeOf[T]

  def echo(msg: String) {
    c.echo(c.enclosingPosition, msg)
  }

  def abort(msg: String): Nothing = {
    c.abort(c.enclosingPosition, msg)
  }

  val sourceTypeName = sourceType.typeSymbol.name

  val sourceTypeNameExpr = c.Expr[String](Literal(Constant(sourceTypeName.toString)))

  val tTypeName = tType.typeSymbol.name
  val tTypeNameExpr = c.Expr[String](Literal(Constant(tTypeName.toString)))

  val tCompanion = tType.typeSymbol.companionSymbol

  def isTAnn(param: Symbol): Boolean = {
    param.annotations.exists(_.tpe =:= annType)
  }

  val matchingParams: List[Symbol] = {
    val params = caseClassParams(sourceType)
    params.filter(isTAnn)
  }

  val listType = c.weakTypeOf[List[_]]
  val listCompanionSymbol = listType.typeSymbol.companionSymbol

  val iterableType = c.weakTypeOf[Iterable[_]]

  val optionType = c.weakTypeOf[Option[_]]


  def paramVal(obj: Tree, param: Symbol): Tree =
    Select(obj, param.name)

  def mkListResult(sourceParams: List[Symbol], obj: Tree): Tree = {

    // transforms the tree types of sourceParams to the Tree's where they're converted
    // to Lists
    val valsAsLists = sourceParams map { sourceParam =>
      val paramValue: Tree = paramVal(obj, sourceParam)

      //      sourceParam.typeSignature

      sourceParam.typeSignature match {
        case paramType if paramType <:< listType =>
          paramValue
        case paramType if paramType <:< iterableType =>
          Select(paramValue, newTermName("toList"))
        case paramType if paramType <:< optionType =>
          Select(paramValue, newTermName("toList"))
        case _ =>
          Apply(
            Select(Ident(listCompanionSymbol), newTermName("apply")),
            List(paramValue)
          )
      }
    }

    Select(
      Apply(
        Select(Ident(listCompanionSymbol), newTermName("apply")),
        valsAsLists
      ),
      newTermName("flatten")
    )
  }

  /**
   * Creates the body of a mapValue method.
   */
  def getValuesBody: c.Expr[List[T]] = {

    echo(s"$sourceType matchingParams: $matchingParams")

    val obj = Ident(newTermName("obj"))
    c.Expr[List[T]](mkListResult(matchingParams, obj))
    //
    //    // The list of trees will pass as arguments to the constructor
    //    val values: List[Tree] = {
    //      // a reference to the TFrom object
    //
    //      // for each of the parameters to TTo.apply, make a tree that Selects the values with the same name
    //      // from the TFrom object
    //
    //      val listTrees = listParams.map {
    //        case (annotationName, param) =>
    //          val sources = extractables(annotationName)
    //          AssignOrNamedArg(Ident(param.name), )
    //      }
    //      val optionTrees = optionParams.map {
    //        case (annotationName, param) =>
    //          AssignOrNamedArg(Ident(param.name),
    //            param.typeSignature match {
    //              case definitions.IntTpe =>
    //                Literal(Constant(0))
    //
    //              case _ =>
    //                Literal(Constant(null))
    //            })
    //
    //      }
    //      requiredTrees.toList ++ optionTrees ++ listTrees
    //    }
    //
    //    showRaw(values)
    //
    //    // make a tree that would call TTo's companion's object apply function with the values
    //    // e.g. TTo.apply(value, value, ...)
    //    val tree: Tree = Apply(constructorTree, values)
    //
    //    // convert the tree to an expression
    //    c.Expr(tree)

    //abort("blaat")
  }

  /**
   * Get the first parameter list from the constructor of a case class
   */
  private def caseClassParams(typ: c.Type): List[Symbol] = {
    val cTor = typ.declaration(nme.CONSTRUCTOR).asMethod
    if (cTor.paramss.size != 1)
      c.abort(c.enclosingPosition, "ObjMapper only supports case classes with 1 parameter list")
    cTor.paramss.head
  }

}
