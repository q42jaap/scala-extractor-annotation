package util.annotatedextractorpoc

import language.experimental.macros

import reflect.macros.Context
import util.objmapper.ObjMapper
import scala.annotation.StaticAnnotation

trait ExtractAnnotation extends StaticAnnotation

object MacrosImpl {

  private def mkHelper[TFrom: c.WeakTypeTag, TTo: c.WeakTypeTag](c: Context) = new Helper[c.type, TFrom, TTo](c) {
    val fromType = c.weakTypeOf[TFrom]
    val toType = c.weakTypeOf[TTo]
  }

  def annotatedExtractorImpl[TFrom: c.WeakTypeTag, TTo: c.WeakTypeTag](c: Context)(): c.Expr[ObjMapper[TFrom, TTo]] = {
    import c.universe._
    val helper = mkHelper[TFrom, TTo](c)

    val body = helper.mapValueBody

    reify {
      new ObjMapper[TFrom, TTo] {
        def mapValue(obj: TFrom): TTo = body.splice
      }
    }
  }
}

private abstract class Helper[C <: Context, TFrom, TTo](val c: C) {

  import c.universe._

  val extractAnnType = c.weakTypeOf[ExtractAnnotation]

  def echo(msg: String) {
    c.echo(c.enclosingPosition, msg)
  }

  def abort(msg: String): Nothing = {
    c.abort(c.enclosingPosition, msg)
  }

  protected def fromType: c.Type

  protected def toType: c.Type

  lazy val fromTypeName = fromType.typeSymbol.name

  lazy val toTypeName = toType.typeSymbol.name

  lazy val toCompanion = toType.typeSymbol.companionSymbol

  lazy val toApplyMethod: MethodSymbol = {
    val applySymbol = toCompanion.typeSignature.declaration(newTermName("apply")) match {
      case NoSymbol => c.abort(c.enclosingPosition, s"No apply function found for $toTypeName")
      case s => s
    }
    applySymbol.asMethod
  }

  lazy val targetParams: Map[String, Symbol] = {
    val params = caseClassParams(toType)
    val targetList = params.map {
      param: Symbol =>
        val annotations = getExtractAnnotationNames(param)
        annotations match {
          case name :: Nil => name -> param
          case Nil => c.abort(param.pos, s"Parameter should have an annotation which extends ${extractAnnType.typeSymbol.name}")
          case lst => c.abort(param.pos, s"Parameter should have just one annotation which extends ${extractAnnType.typeSymbol.name}")
        }
    }
    targetList.groupBy(_._1).foreach { tplListTpl: (String, List[(String, Symbol)]) =>
      if (tplListTpl._2.size > 1)
        c.abort(tplListTpl._2.last._2.pos, s"In the type $toTypeName there should be just 1 ${tplListTpl._1} annotation")
    }
    targetList.toMap
  }


  def getExtractAnnotationNames(param: Symbol): List[String] = {
    param.annotations.collect {
      case ann if ann.tpe <:< extractAnnType =>
        ann.tpe.typeSymbol.name.toString
    }
  }

  lazy val extractables: Map[String, List[Symbol]] = {
    val params = caseClassParams(fromType)
    val extrList = params.map {
      param: Symbol =>
        val annotations = getExtractAnnotationNames(param)
        annotations.map(_ -> param)
    }
    extrList.flatten.groupBy(_._1).mapValues(_.map(t => t._2))
  }

  def isRequiredParam(toParam: Symbol): Boolean = {
    val paramType = toParam.typeSignature
    val isList = paramType <:< c.weakTypeOf[Iterable[_]]
    val isOpt = paramType <:< c.weakTypeOf[Option[_]]
    !(isList || isOpt)
  }

  /**
   * Creates the body of a mapValue method.
   */
  def mapValueBody: c.Expr[TTo] = {
    // get a reference to the companion's apply method
    // we'll use this for reflection only
    val cTor: MethodSymbol = toApplyMethod

    // select the apply method from the companion object
    // we need the tree which you would've typed yourself, we cant just use the methodSymbol (for some reason)
    val constructorTree: Tree = Select(Ident(newTermName(toCompanion.name.toString)), newTermName("apply"))

    echo(s"$fromType extractables: ${extractables}")

    targetParams

    val requiredParams = targetParams.filter(kv => isRequiredParam(kv._2))
    echo(s"$toType requiredParams: ${requiredParams}")
    val optionalParams = targetParams.filterNot(kv => isRequiredParam(kv._2))
    echo(s"$toType optionalParams: ${optionalParams}")

    val missingRequiredParams = requiredParams.filter {
      reqParamKv =>
        val result = !extractables.contains(reqParamKv._1)
        if (result)
          c.error(reqParamKv._2.pos, s"Required param is not extrable from $fromTypeName")
        result
    }

    if (!missingRequiredParams.isEmpty)
      abort(s"Missing required properties for mapping from $fromTypeName")


    //abort("blaat")
    // The list of trees will pass as arguments to the constructor
    val values: List[Tree] = {
      // we only support constructors with 1 parameterlist, this is already checked in checkSuperSet
      val params: List[Symbol] = cTor.paramss.head

      // a reference to the TFrom object
      val obj = Ident(newTermName("obj"))

      // for each of the parameters to TTo.apply, make a tree that Selects the values with the same name
      // from the TFrom object
      params.map {
        case param if param.typeSignature == definitions.IntTpe =>
          Literal(Constant(0))
        case param =>
          Literal(Constant(null))
      }
    }

    // make a tree that would call TTo's companion's object apply function with the values
    // e.g. TTo.apply(value, value, ...)
    val tree: Tree = Apply(constructorTree, values)

    // convert the tree to an expression
    c.Expr(tree)
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

  /**
   * Checks whether TTo is a subset of TFrom
   */
  def checkSubSuperSet = {
    // Retrieve the params for the From Type
    val fromParams = caseClassParams(fromType)

    // Find the parameters of the Published case class
    val toParams = caseClassParams(toType)

    // Find the first parameter name that doesn't match
    val wrongProperties = toParams.filter {
      toParam =>
        val hasFromBrother = fromParams.exists {
          fromParam =>
            toParam.name.equals(fromParam.name) && toParam.typeSignature.equals(fromParam.typeSignature)
        }
        !hasFromBrother
    }

    if (!wrongProperties.isEmpty) {
      val wrongPropertieNames = wrongProperties.map(_.name).mkString(",")
      c.abort(c.enclosingPosition, s"Could not create ObjMapper[$fromTypeName, $toTypeName], properties from $toTypeName don't match: ${wrongPropertieNames}")
    }
  }
}
