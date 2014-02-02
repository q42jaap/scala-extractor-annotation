package util.annotatedextractorpoc

/**
 * Extracts instances of T that are annotated by TAnn from TSource
 * @tparam TSource
 * @tparam T
 * @tparam TAnn
 */
trait Extractor[TSource, T, TAnn <: ExtractAnnotation] {
  def getValues(obj: TSource): List[T]
}

class ExtractorBase[TAnn <: ExtractAnnotation] {

  def getValues[TSource, T](obj: TSource)(implicit extractor: Extractor[TSource, T, TAnn]): List[T] = {
    extractor.getValues(obj)
  }

}