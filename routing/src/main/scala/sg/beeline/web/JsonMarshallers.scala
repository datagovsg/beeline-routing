package sg.beeline.web

import akka.http.scaladsl.marshalling.PredefinedToEntityMarshallers
import akka.http.scaladsl.model.{HttpCharsets, MediaType}
import akka.http.scaladsl.unmarshalling.{PredefinedFromEntityUnmarshallers, Unmarshaller}
import io.circe.Json

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
}
