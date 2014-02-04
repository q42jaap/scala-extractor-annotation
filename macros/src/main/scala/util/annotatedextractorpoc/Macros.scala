package util.annotatedextractorpoc

import language.experimental.macros
import util.objmapper.ObjMapper
import scala.annotation.StaticAnnotation


trait ExtractAnnotation extends StaticAnnotation


object Macros {

  def extractor[TSource, T, TAnn <: ExtractAnnotation](): Extractor[TSource, T, TAnn] = macro MacrosImpl.extractorImpl[TSource, T, TAnn]

}
