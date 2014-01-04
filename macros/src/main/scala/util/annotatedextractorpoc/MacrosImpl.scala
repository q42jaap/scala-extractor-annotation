package util.annotatedextractorpoc

import language.experimental.macros

import reflect.macros.Context
import util.objmapper.ObjMapper
import scala.annotation.StaticAnnotation

class extract(val name: String) extends StaticAnnotation

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

  def echo(msg: String) {
    c.echo(c.enclosingPosition, msg)
  }

  def abort(msg: String): Nothing = {
    c.abort(c.enclosingPosition, msg)
  }

  protected def fromType: c.Type

  protected def toType: c.Type

  private def fromTypeName = fromType.typeSymbol.name

  private def toTypeName = toType.typeSymbol.name

  private def toCompanion = toType.typeSymbol.companionSymbol

  private def toApplyMethod: MethodSymbol = {
    val applySymbol = toCompanion.typeSignature.declaration(newTermName("apply")) match {
      case NoSymbol => c.abort(c.enclosingPosition, s"No apply function found for $toTypeName")
      case s => s
    }
    applySymbol.asMethod
  }


  def getExtractValue(tree: Tree): String = ???


  private def extractables: Map[String, List[Symbol]] = {
    val params = caseClassParams(fromType)
    val extrList = params.map {
      param: Symbol =>
        param.annotations.collect {
          case ann if ann.tpe <:< c.weakTypeOf[extract] =>
            //        val args = extr.scalaArgs
            val args = ann.javaArgs
            val arg0 = args.get(newTermName("name")).getOrElse(abort("@extract() should have exactly 1 parameter"))

            val name = arg0 match {
              case litArg: LiteralArgument => litArg.value.value.asInstanceOf[String]
              case _ => abort(s"unsuported use of @extract: $arg0")
            }
            echo(s"args @extract(name = $name)")
            name -> param
        }
    }
    extrList.flatten.groupBy(_._1).mapValues(_.map(t => t._2))
  }

  private def extractablesScala: Map[String, List[Symbol]] = {
    val params = caseClassParams(fromType)
    val extrList = params.map {
      param: Symbol =>
        param.annotations.collect {
          case ann if ann.tpe <:< c.weakTypeOf[extract] =>
            val args = ann.scalaArgs
            val arg0 = args.head
            val arg0expr = c.Expr(arg0)
            val name: String = c.eval(arg0expr)
            echo(s"args @extract(name = $name)")
            name -> param
        }
    }
    extrList.flatten.groupBy(_._1).mapValues(_.map(t => t._2))
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

    val extrs = extractablesScala
    c.echo(c.enclosingPosition, s"$fromType extractables: ${extrs}")

    c.abort(c.enclosingPosition, "blaat")
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
