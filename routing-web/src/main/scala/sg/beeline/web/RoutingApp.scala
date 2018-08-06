package sg.beeline.web

import sg.beeline.io.{BuiltIn, Import}
import sg.beeline.util.Geo

object RoutingApp extends App {
  import akka.actor._
  import akka.stream.ActorMaterializer
  import akka.http.scaladsl.Http

  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher
  implicit val e2eAuthSettings = DefaultE2EAuthSettings

  Geo.initialize()

  val bindingFuture = Http().bindAndHandle(
    new IntelligentRoutingService(BuiltIn, Import.getLiveRequests).myRoute,
    "0.0.0.0",
    scala.util.Properties.envOrElse("PORT", "8080").toInt
  )
}
