package domain

import org.specs2.mutable._
import play.api.test._
import play.api.test.Helpers._
import scala.reflect.runtime.universe._
import domain.core._
import scala.collection.mutable
import scala.collection.immutable.ListMap
import reactivemongo.bson.BSONObjectID
import org.specs2.execute.{Result}

class PublishTest extends Specification {

  // The actual tests
  "Publishable" should {
    "have Published with all @PubRemove parameters removed" in {
      checkPublishableType[CMSClass1]
    }

    "have Published with all @PubRequire Option type parameters unwrapped" in {
      checkPublishableType[CMSClass2]
    }
  }

  // These tests test the test code.
  "checkPublishableType" should {
    "should throw an Exception if @PubRemove annotated params is not removed in Published" in {
      checkPublishableType[CMSClass3] must throwAn[Exception]
    }

    "should throw an Exception if Publishable params have different order than Published params" in {
      checkPublishableType[CMSClass4] must throwAn[Exception]
    }

    "should throw an Exception if Published has params not declared in Publishable" in {
      checkPublishableType[CMSClass5] must throwAn[Exception]
    }

    "should throw an Exception if @PubRequire Option[T] param in Publishable doesn't have an T type in Published" in {
      checkPublishableType[CMSClass6] must throwAn[Exception]
    }

    "should throw an Exception if Publishable and Published params types don't match" in {
      checkPublishableType[CMSClass7] must throwAn[Exception]
    }
  }

  // Check integrity of the given Publishable and its Published counterpart
  def checkPublishableType[T <: PublishableTmp[_]: TypeTag]: Result = {
    val cmsType = typeOf[T]

    // Retrieve the params for the cms type
    val cmsParams = caseClassParamsOf(cmsType)

    // Find the matching Published type for the given Publishable type
    val pubTypeOption = findBaseTypeParam[PublishableTmp[_]](cmsType)
    val pubType = pubTypeOption.getOrElse(throw new Exception("No Publishable base type for " + cmsType))

    // Find the parameters of the Published case class
    val pubParams = caseClassParamsOf(pubType)

    // Remove all cms params that have a PubRemove annotation
    val filteredCMSParams = cmsParams.filterNot {
      case (name: String, typ: Type, annos: List[Annotation]) => annos.exists {
        case anno => anno.tpe == typeOf[PubRemove]
      }
    }

    // Zip both param lists to compare them
    val zippedParams = filteredCMSParams.zip(pubParams)

    // Find the first parameter name that doesn't match
    zippedParams.map {
      case ((cmsName, cmsTyp, cmsAnnos), (pubName, pubTyp, pubAnnos)) =>
        if (cmsName != pubName)
        {
          // See if this param was filtered out due to the PubRemove annotation
          val wasCMSOnly = cmsParams.exists {
            case (name: String, typ: Type, annos: List[Annotation]) => annos.exists {
              case anno => anno.tpe == typeOf[PubRemove] && name == pubName
            }
          }

          if (wasCMSOnly)
            throw new Exception("@PubRemove was set on '" + pubName + "' but it wasn't omitted from the Published class.")
          else
            throw new Exception("Names are not equal for param: cms: '" + cmsName + "' != published: '" + pubName + "'")
        }
    }

    // Find the first parameter type that doesn't match
    zippedParams.map {
      case ((cmsName, cmsTyp, cmsAnnos), (pubName, pubTyp, pubAnnos)) =>
        // Either the types are equal...
        val typesEq = cmsTyp == pubTyp

        // ...or the param is Required.
        val cmsRequired = cmsAnnos.exists {
          case anno => anno.tpe == typeOf[PubRequire]
        }

        // Then the cmsType must be Option[pubType]
        val optionTypeEq = pubTyp == findBaseTypeParam[Option[_]](cmsTyp).getOrElse(None)

        if (cmsRequired && !optionTypeEq)
          throw new Exception("@CMSRequired was set on: '" + cmsName + "' but cms type: '" + cmsTyp + "' is not an Option of published type: '" + pubTyp + "'")

        if (!cmsRequired && !typesEq)
          throw new Exception("Types don't match for param: '" + cmsName + "' cms type: '" + cmsTyp + "' published type: '" + pubTyp + "'")
    }

    // Maybe the published class has extra params at the end that were dropped by zip.
    if (pubParams.length > filteredCMSParams.length)
    {
      val superflousParams = pubParams.takeRight(pubParams.length - filteredCMSParams.length)
      throw new Exception("Too many parameters declared on published type: " + superflousParams)
    }

    Result.unit()
  }

  // Generate a List of (name, type, annotations) tuples for all parameters of the given case class type
  def caseClassParamsOf(typ: Type) = {
    val cTor = typ.declaration(nme.CONSTRUCTOR).asMethod
    cTor.paramss.flatten.map {
      sym => (sym.name.toString, sym.typeSignature, sym.annotations)
    }
  }

  // Find the (only!) parameter type P for the given base type B of the given type T
  // E.g. T extends B[P]
  def findBaseTypeParam[T: TypeTag](typ: Type): Option[Type] = {
    typ.baseType(typeOf[T].typeSymbol) match {
      case TypeRef(_, _, List(tpe)) => Some(tpe)
      case _ => None
    }
  }
}

case class CMSClass1(
                      _id: BSONObjectID, @PubRemove cmsData: PublishingCmsData
                      ) extends PublishableTmp[PubClass1]

case class PubClass1(
                      _id: BSONObjectID
                      ) extends Published

case class CMSClass2(
                      _id: BSONObjectID, @PubRemove cmsData: PublishingCmsData,
                      @PubRequire someParam: Option[String]
                      ) extends PublishableTmp[PubClass2]

case class PubClass2(
                      _id: BSONObjectID, someParam: String
                      ) extends Published

case class CMSClass3(
                      _id: BSONObjectID, @PubRemove cmsData: PublishingCmsData
                      ) extends PublishableTmp[PubClass3]

case class PubClass3(
                      _id: BSONObjectID, cmsData: PublishingCmsData
                      ) extends Published

case class CMSClass4(
                      _id: BSONObjectID, cmsData: PublishingCmsData
                      ) extends PublishableTmp[PubClass4]

case class PubClass4(
                      cmsData: PublishingCmsData, _id: BSONObjectID
                      ) extends Published

case class CMSClass5(
                      _id: BSONObjectID, cmsData: PublishingCmsData
                      ) extends PublishableTmp[PubClass5]

case class PubClass5(
                      _id: BSONObjectID, cmsData: PublishingCmsData, pubOnlyParam: Int
                      ) extends Published

case class CMSClass6(
                      _id: BSONObjectID, cmsData: PublishingCmsData, @PubRequire required: Option[String]
                      ) extends PublishableTmp[PubClass6]

case class PubClass6(
                      _id: BSONObjectID, cmsData: PublishingCmsData, required: Option[String]
                      ) extends Published

case class CMSClass7(
                      _id: BSONObjectID, cmsData: PublishingCmsData, typeMismatch: String
                      ) extends PublishableTmp[PubClass7]

case class PubClass7(
                      _id: BSONObjectID, cmsData: PublishingCmsData, typeMismatch: Int
                      ) extends Published

