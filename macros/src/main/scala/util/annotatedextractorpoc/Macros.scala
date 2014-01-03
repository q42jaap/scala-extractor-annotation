package util.annotatedextractorpoc

import language.experimental.macros
import util.objmapper.ObjMapper

object Macros {

  def annotatedExtractor[TFrom, TTo](): ObjMapper[TFrom, TTo] = macro MacrosImpl.annotatedExtractorImpl[TFrom, TTo]

}
