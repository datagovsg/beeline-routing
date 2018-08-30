package sg.beeline.web

import akka.http.scaladsl.marshalling.{Marshaller, PredefinedToEntityMarshallers}
import akka.http.scaladsl.model.{HttpCharsets, HttpEntity, MediaType, MessageEntity}
import akka.http.scaladsl.unmarshalling.{PredefinedFromEntityUnmarshallers, Unmarshaller}
import io.circe.{Decoder, Encoder, Json}

trait JsonMarshallers {

  implicit val jsonMarshaller = PredefinedToEntityMarshallers.stringMarshaller(
    MediaType.applicationWithFixedCharset(
      "json",
      HttpCharsets.`UTF-8`)
    )
    .compose(
      (json: Json) => _root_.io.circe.Printer.noSpaces.pretty(json)
    )

  /* Needed to unmarshall JSON in Query Params */
  implicit val stringToJsonUnmarshaller = Unmarshaller.strict[String, Json](s =>
    _root_.io.circe.parser.parse(s) match {
      case Right(t) => t
      case Left(e) => throw e
    })

  implicit val entityToJsonUnmarshaller = PredefinedFromEntityUnmarshallers
    .stringUnmarshaller.map(s =>
    _root_.io.circe.parser.parse(s) match {
      case Right(t) => t
      case Left(e) => throw e
    })

  object objectJsonMarshallers {
    // Convert any JSON entity to a T, if a Decoder[T] exists
    implicit def entityToObjectViaDecoderUnmarshaller[T](implicit decoder: Decoder[T]): Unmarshaller[HttpEntity, T] =
      entityToJsonUnmarshaller.map(_.as[T] match {
        case Right(t) => t
        case Left(exc) => throw exc
      })

    // Convert any T to a JSON, if a Encoder[T] exists
    implicit def objectToEntityViaEncoderMarshaller[T](implicit encoder: Encoder[T]): Marshaller[T, MessageEntity] =
      jsonMarshaller.compose[T] { o: T => encoder(o) }
  }
}
