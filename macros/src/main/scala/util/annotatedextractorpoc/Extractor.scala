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

trait DefaultExtractors {

  implicit def traversableExtractor[TSource, Repr <% Traversable[TSource], T, TAnn <: ExtractAnnotation](implicit extractor: Extractor[TSource, T, TAnn]): Extractor[Repr, T, TAnn] = {
    new TraversableExtractor[TSource, Repr, T, TAnn]
  }

}

class TraversableExtractor[TSource, Repr <% Traversable[TSource], T, TAnn <: ExtractAnnotation](implicit extractor: Extractor[TSource, T, TAnn]) extends Extractor[Repr, T, TAnn] {
  def getValues(objs: Repr): List[T] = {
    objs.flatMap(obj => extractor.getValues(obj)).toList
  }
}