package util.annotatedextractorpoc

import language.experimental.macros

import reflect.macros.Context
import util.objmapper.ObjMapper
import scala.annotation.StaticAnnotation

trait ExtractAnnotation extends StaticAnnotation

abstract class ExtractorHelper[TFrom, TTo] extends ObjMapper[TFrom, TTo] {
  val fromTypeName: String
  val toTypeName: String

  private def mkError(msg: String): IllegalStateException =
    new IllegalStateException(s"Something error happen while mapping from $fromTypeName to $toTypeName: $msg")

  def mkList[A](vals: List[_]): List[A] = {
    val values = vals flatMap {
      case it: Iterable[Option[A]] => it
      case opt: Option[A] => List(opt)
      case it: Iterable[A] => it.map(v => Some(v))
      case v: A => List(Some(v))
      case _ => throw mkError("Value has illegal type")
    }
    values.flatten
  }

  def mkOpt[A](vals: List[_]): Option[A] = {
    val values = mkList[A](vals)

    values match {
      case Nil => None
      case v :: Nil => Some(v)
      case _ => throw mkError("Option value has more than 1 values")
    }
  }

  def mkVal[A](vals: List[_]): A = {
    val values = mkList[A](vals)

    values match {
      case Nil => throw mkError("Value is not available")
      case v :: Nil => v
      case _ => throw mkError("There are more than 1 valua available")
    }
  }
}

object MacrosImpl {

  private def mkHelper[TFrom: c.WeakTypeTag, TTo: c.WeakTypeTag](c: Context): Helper[c.type, TFrom, TTo] = new Helper[c.type, TFrom, TTo](c) {
    val fromType = c.weakTypeOf[TFrom]
    val toType = c.weakTypeOf[TTo]
  }

  def annotatedExtractorImpl[TFrom: c.WeakTypeTag, TTo: c.WeakTypeTag](c: Context)(): c.Expr[ObjMapper[TFrom, TTo]] = {
    import c.universe._
    val helper = mkHelper[TFrom, TTo](c)

    helper.checkExtractableTypes()

    val body = helper.mapValueBody

    reify {
      new ExtractorHelper[TFrom, TTo] {
        val fromTypeName = helper.fromTypeName.toString
        val toTypeName = helper.toTypeName.toString

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
    targetList.groupBy(_._1).foreach {
      tplListTpl: (String, List[(String, Symbol)]) =>
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

  def isListParam(targetParam: Symbol): Boolean = {
    val paramType = targetParam.typeSignature
    paramType <:< c.weakTypeOf[List[_]]
  }

  def isRequiredParam(targetParam: Symbol): Boolean = {
    !(isOptionParam(targetParam) || isListParam(targetParam))
  }

  def isOptionParam(targetParam: Symbol) : Boolean = {
    val paramType = targetParam.typeSignature
    paramType <:< c.weakTypeOf[Option[_]]
  }

  def checkExtractableTypes() {
    // TODO: check whether extractables and targetParams are
    // type compatible. Consider List/Iterable and Options
    // when the target has an Option[A] the source can be a List[A] etc.

    // also: for a property that is required (neither Optional nor Iterable)
    // we need to check that there can't be too much extractables for it.
    // e.g. case class FromClass(@smthng prop1: String, @smthng prop2: String)
    // can never be mapped to ToClass(@smthng prop: String), prop needs to be List[String]

    val missingRequiredParams = requiredParams.filter {
      reqParamKv =>
        val result = !extractables.contains(reqParamKv._1)
        if (result)
          c.error(reqParamKv._2.pos, s"Required param is not extrable from $fromTypeName")
        result
    }

    if (!missingRequiredParams.isEmpty)
      abort(s"Missing required properties for mapping from $fromTypeName")

    //TODO check for optionParams where there is Option[_]
  }

  /**
   * required parameters are parameter that don't have the type Option[_] or List[_]
   * They will need to be present at any time.
   */
  lazy val requiredParams = targetParams.filter(kv => isRequiredParam(kv._2))
  lazy val optionParams = targetParams.filter(kv => isOptionParam(kv._2))
  lazy val listParams = targetParams.filter(kv => isListParam(kv._2))

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

    echo(s"$toType requiredParams: ${requiredParams}")
    echo(s"$toType optionParams: ${optionParams}")
    echo(s"$toType listParams: ${listParams}")



    // The list of trees will pass as arguments to the constructor
    val values: List[Tree] = {
      // a reference to the TFrom object
      val obj = Ident(newTermName("obj"))
      val requiredValues: Map[String, Tree] = ???
      val optionalValues: Map[String, Tree] = ???

      // for each of the parameters to TTo.apply, make a tree that Selects the values with the same name
      // from the TFrom object

      val requiredTrees = requiredParams.map {
        case (name, param) =>
          AssignOrNamedArg(Ident(newTermName(name)),
            param.typeSignature match {
              case definitions.IntTpe =>
                Literal(Constant(0))

              case _ =>
                Literal(Constant(null))
            })
      }
      val optionalTrees = (optionParams ++ listParams).map {
        case (name, param) =>
          AssignOrNamedArg(Ident(newTermName(name)),
            param.typeSignature match {
              case definitions.IntTpe =>
                Literal(Constant(0))

              case _ =>
                Literal(Constant(null))
            })

      }
      requiredTrees.toList ++ optionalTrees
    }

    // make a tree that would call TTo's companion's object apply function with the values
    // e.g. TTo.apply(value, value, ...)
    val tree: Tree = Apply(constructorTree, values)

    // convert the tree to an expression
    c.Expr(tree)

    abort("blaat")
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
