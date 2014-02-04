scala-extractor-annotation
==========================
A scala macro which generates an Extractor.

Use case
========
Suppose you have a document database where you store your domain models as JSON (or if you're using mongodb it might be BSON). Sometimes you would like to write generic code over all the different types of documents you put into the database.

Your code might look like this:

    case class Image(id: Int, url: String)
    
    case class ImageRef(@imageId id: Int, alt: String)
    
    case class HomePage(
      id: Int,
      header: ImageRef,
      thumbnail: Int,
      contents: String
    )
        
    case class AboutPage(
      id: Int,
      illustration: Int,
      contents: String
    )

So the `thumbnail` and `illustration` fields are "foreign keys" to the image table/collection in your database. The ImageRef also has the `id` field that refers to the image column.

Suppose we need to know which images we're referring to. Each time we save the pages to the database, we add this information to a separate "column". So we make a trait `RefersToImages` which defines a def `imageIds` and we just implement this method. The implementation for `HomePage` looks like this:

    case class HomePage(
      id: Int,
      header: ImageRef,
      thumbnail: Int,
      contents: String
    ) {
      def imageIds = List(header.id, thumbnail)
    }
    
Suppose we add an optional `illustration` to `Homepage`, the imageIds implementation gets bigger:

    case class HomePage(
      id: Int,
      header: ImageRef,
      thumbnail: Int,
      illustration: Option[ImageRef],
      contents: String
    ) {
      def imageIds = List(header.id, thumbnail) ++ illustration.map(_.id).toList
    }

The implementation of imageIds needs to be in sync with the fields, which a lazy (=good) programmer doesn't like.

I wanted to create a Macro that extracts all the `imageId`s from the `HomePage` by looking at the case class members. Since the `imageId`s happend to be of type `Int` we cannot just look for a type. So instead we annotate the members with an annotation `@imageId` (using a value class instead of annotating members might be an option, but I choose not to do that because users might not like this, I might refactor)

The macro will find all the annotated members at compile time and generate a function that can create the `List[T]` (`List[Int]` in this example). It can also do this for "nested" case classes by using implicit extractors.

The models now look like this (you define the annotation yourself):

    class imageId extends ExtractAnnotation
    
    case class Image(id: Int, url: String)
    
    case class ImageRef(@imageId id: Int, alt: String)
    
    case class HomePage(
      id: Int,
      bigLogo: ImageRef,
      @imageId thumbnail: Int,
	    illustration: Option[ImageRef],
      contents: String
    )
        
    case class AboutPage(
      id: Int,
      @imageId illustration: Int,
      contents: String
    )


The extractor trait looks like this:

    trait Extractor[TSource, T, TAnn <: ExtractAnnotation] {
      def getValues(obj: TSource): List[T]
    }

As promised, generating extractors is very straight forward:

    object ImageIdExtractor extends ExtractorBase[imageId] {
      implicit val eImageRef = Macros.extractor[ImageRef, Int, imageId]
      implicit val eHomePage = Macros.extractor[HomePage, Int, imageId]
    }

Getting the actual `List[Int]` is as easy as calling the generic getValues method on the `ImageIdExtractor`:

    val ids: List[Int] = ImageIdExtractor.getValues(homePage)
    val ids: List[Int] = ImageIdExtractor.getValues(aboutPage)


Disclaimer
==========
The project is not yet finished, things will be changed... a lot!
