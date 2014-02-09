package util.annotatedextractorpoc

import language.experimental.macros

import reflect.macros.Context

object MacrosImpl {

  private def mkHelper[TSource: c.WeakTypeTag, T: c.WeakTypeTag, TAnn <: ExtractAnnotation : c.WeakTypeTag]
  (c: Context): Helper[c.type, TSource, T, TAnn] = new Helper[c.type, TSource, T, TAnn](c) {
  }

  def extractorImpl[TSource: c.WeakTypeTag, T: c.WeakTypeTag, TAnn <: ExtractAnnotation : c.WeakTypeTag]
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


  private abstract class Helper[C <: Context, TSource: C#WeakTypeTag, T: C#WeakTypeTag, TAnn <: ExtractAnnotation : C#WeakTypeTag](val c: C) {

    import c.universe._

    val extractorType = c.weakTypeOf[Extractor[_, _, _]]

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

    def hasTAnn(param: Symbol): Boolean = {
      param.annotations.exists(_.tpe =:= annType)
    }
    val sourceParams = caseClassParams(sourceType)

    val annotatedParams: List[Symbol] = sourceParams.filter(hasTAnn)

    def isCaseClass(param: Symbol): Boolean = {
      param.typeSignature <:< c.weakTypeOf[Product]
    }

    def isIterableParamWithCaseClass(param: Symbol): Boolean = {
      param.typeSignature <:< iterableType
      //TODO check type argument isCaseClass
    }

    def isOptionParamWithCaseClass(param: Symbol): Boolean = {
      param.typeSignature <:< optionType
    }

    val otherParams: List[Symbol] = {
      sourceParams.filterNot(hasTAnn)
    }

    def getImplicitExtractorType(paramSourceType: c.Type): c.Type = {
      appliedType(extractorType, List(paramSourceType, tType, annType))
    }

    //  val deepIterableParams = otherParams.filter { p =>
    //    p.typeSignature <:< iterableType
    //  }
    //
    //  val deepOptionParams = otherParams.filter { p =>
    //    p.typeSignature <:< optionType
    //  }
    //
    //  val deepCaseClassParams = otherParams.filter(isCaseClass)


    val listType = c.weakTypeOf[List[_]]
    val listOfTType = appliedType(listType, List(tType))
    val listCompanionSymbol = listType.typeSymbol.companionSymbol

    val iterableType = c.weakTypeOf[Iterable[_]]
    val iterableOfTType = appliedType(iterableType, List(tType))

    val optionType = c.weakTypeOf[Option[_]]
    val optionOfTType = appliedType(optionType, List(tType))


    //val reader = c.inferImplicitValue(appliedType(readerType, List(A)))
    //if(! reader.isEmpty)

    def paramVal(obj: Tree, param: Symbol): Tree =
      Select(obj, param.name)

    def mkListResult(obj: Tree): Tree = {

      // transforms the tree types of annotatedParams to the Tree's where they're converted
      // to Lists
      val valsAsLists: List[Tree] = annotatedParams map { sourceParam =>
        val paramValue: Tree = paramVal(obj, sourceParam)

        //      sourceParam.typeSignature

        sourceParam.typeSignature match {
          case paramType if paramType <:< listOfTType =>
            //echo(s"$sourceParam: List[T]")
            paramValue
          case paramType if paramType <:< iterableOfTType =>
            //echo(s"$sourceParam: Iterable[T]")
            Select(paramValue, newTermName("toList"))
          case paramType if paramType <:< optionOfTType =>
            //echo(s"$sourceParam: Option[T]")
            Select(paramValue, newTermName("toList"))
          case paramType if paramType <:< tType =>
            //echo(s"$sourceParam: T")
            Apply(
              Select(Ident(listCompanionSymbol), newTermName("apply")),
              List(paramValue)
            )
          case _ =>
            abort(s"parameter ${sourceParam.name} of type $sourceTypeName with type ${sourceParam.typeSignature} is not compatible with ${tType}")
        }
      }

      val deepLists: List[Option[Tree]] = otherParams map { otherParam =>
        val implicitExtractorType = getImplicitExtractorType(otherParam.typeSignature)
        val extractorTree: Tree = c.inferImplicitValue(implicitExtractorType)
        echo(s"otherParam: $otherParam, $implicitExtractorType ${showRaw(extractorTree)}")
        if (extractorTree.isEmpty) {
          //TODO: when do we need to raise an error?
          None
        } else {
          val getValuesFunc = Select(extractorTree, newTermName("getValues"))
          Some(Apply(getValuesFunc, List(paramVal(obj, otherParam))))
        }
      }



      Select(
        Apply(
          Select(Ident(listCompanionSymbol), newTermName("apply")),
          valsAsLists ++ deepLists.flatten
        ),
        newTermName("flatten")
      )
    }

    /**
     * Creates the body of a mapValue method.
     */
    def getValuesBody: c.Expr[List[T]] = {


      echo(s"$sourceType matchingParams: $annotatedParams")
      echo(s"$sourceType deepParams: $otherParams")

      val obj = Ident(newTermName("obj"))
      val tree = mkListResult(obj)
      echo(s"TREE: ${showRaw(tree)}")
      c.Expr[List[T]](tree)

      //abort("blaat")
    }

    /**
     * Get the first parameter list from the constructor of a case class
     */
    private def caseClassParams(typ: c.Type): List[Symbol] = {
      val cTor = typ.declaration(nme.CONSTRUCTOR).asMethod
      if (cTor.paramss.size != 1)
        c.abort(c.enclosingPosition, "Extractor only supports case classes with 1 parameter list")
      cTor.paramss.head
    }

  }

}
