package sg.beeline.ruinrecreate

import com.amazonaws.services.lambda.AWSLambdaAsyncClientBuilder
import com.amazonaws.services.lambda.model.{InvokeRequest, InvokeResult}
import io.circe.Decoder.Result
import io.circe.{Decoder, Encoder, HCursor, Json}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.jawn.decodeByteBuffer
import sg.beeline.io.Import
import sg.beeline.problem.Request.{RequestFromSuggestion, RequestOverrideTime}
import sg.beeline.problem._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object BeelineSuggestRouteService {
  case class OD(
                 origin: BusStop,
                 destination: BusStop
               )
  case class SuggestRouteInput(
                                seedRequest: Request,
                                od: OD,
                                requests: List[Request]
                              )
  case class SuggestRouteOutput(
                                 routes: Route
                               )
}

trait BeelineSuggestRouteService {
  import BeelineSuggestRouteService._

  val problem = new BasicRoutingProblem(
    List(),
    // TODO: get from settings / env var
    300.0, // ...getOrElse(300.0)
    300.0,
    dataSource = Import
  )

  /* Decoders */
  implicit val busStopDecoder = deriveDecoder[BusStop]
  implicit val suggestionDecoder = deriveDecoder[Suggestion]

  implicit val requestDecoder = new Decoder[Request] {
    override def apply(c: HCursor): Result[Request] =
      for {
        suggestion <- c.as[Suggestion]
      } yield new RequestFromSuggestion(suggestion, problem, Import)
  }

  implicit val suggestRouteInputDecoder: Decoder[SuggestRouteInput] = {
    implicit val ODDecoder = deriveDecoder[OD]
    deriveDecoder[SuggestRouteInput]
  }

  implicit val activityDecoder = new Decoder[Activity] {
    implicit val pickupDecoder = deriveDecoder[Pickup]
    implicit val dropoffDecoder = deriveDecoder[Dropoff]
    override def apply(c: HCursor): Result[Activity] = {
      for {
        activityType <- c.downField("type").as[String]

        activity <- activityType match {
          case "pickup" => c.as[Pickup]
          case "dropoff" => c.as[Dropoff]
          case "end" => Right(EndActivity())
          case "start" => Right(StartActivity())
        }
      } yield activity
    }
  }

  implicit val routeDecoder = new Decoder[Route] {
    override def apply(c: HCursor): Result[Route] = {
      println(c.value)
      for {
        _activities <- c.downField("_activities").as[List[Activity]]
        time <- c.downField("time").as[Double]
      } yield(new Route(problem, _activities, time))
    }
  }

  /* Encoders */
  implicit val busStopEncoder = deriveEncoder[BusStop]
  implicit val ODEncoder = deriveEncoder[OD]

  implicit val suggestionEncoder = deriveEncoder[Suggestion]
  implicit val requestFromSuggestionEncoder = new Encoder[RequestFromSuggestion] {
    override def apply(a: RequestFromSuggestion): Json = {
      // suggestion, routingproblem, datasource
      import io.circe.syntax._
      a.suggestion.asJson
    }
  }

  implicit val requestEncoder: Encoder[Request] = new Encoder[Request] {
    import _root_.io.circe.syntax._

    implicit val requestOverrideTimeEncoder = new Encoder[RequestOverrideTime] {
      import io.circe.syntax._
      override def apply(a: RequestOverrideTime): Json = {
        a.r.asJson(requestEncoder)
          .mapObject(o => o.add("time", a.time.asJson))
      }
    }

    override def apply(a: Request): Json = a match {
      case a: RequestFromSuggestion => a.asJson
      case a: RequestOverrideTime => a.asJson
      case a: BasicRequest =>
        new Suggestion(
          a.id,
          a.start,
          a.end,
          a.time,
          a.weight
        ).asJson
    }
  }


  implicit val activityEncoder = new Encoder[Activity] {
    implicit val pickupEncoder = deriveEncoder[Pickup]
    implicit val dropoffEncoder = deriveEncoder[Dropoff]
    implicit val startActivityEncoder = deriveEncoder[StartActivity]
    implicit val endActivityEncoder = deriveEncoder[EndActivity]

    override def apply(a: Activity): Json = {
      val (activityType, json) = a match {
        case a: Pickup => "pickup" -> pickupEncoder(a)
        case a: Dropoff => "dropoff" -> dropoffEncoder(a)
        case a: StartActivity => "start" -> startActivityEncoder(a)
        case a: EndActivity => "end" -> endActivityEncoder(a)
      }

      json.mapObject(o => o.add("type", Json.fromString(activityType)))
    }
  }

  implicit val routeEncoder = new Encoder[Route] {
    import io.circe.syntax._
    override def apply(a: Route): Json = Json.obj(
      // _activities : Seq[Activity]
      "_activities" -> a.activities.asJson,
      "time" -> a.time.asJson
    )
  }

  implicit val suggestRouteInputEncoder = deriveEncoder[SuggestRouteInput]
  implicit val suggestRouteOutputEncoder = deriveEncoder[SuggestRouteOutput]
  implicit val suggestRouteOutputDecoder = deriveDecoder[SuggestRouteOutput]

  import io.circe.syntax._
  import io.circe.parser._

  def executeLambda(
                     request: Request,
                     od: (BusStop, BusStop),
                     requests: List[Request])
                    (implicit executionContext: ExecutionContext): Future[Route] = {

    val suggestRouteInput = SuggestRouteInput(request, OD(od._1, od._2), requests)

    requestWithPayload(suggestRouteInput.asJson.toString)
      .map(json => json.as[Route] match {
        case Right(route) => route
        case Left(exc) => throw exc
      })
  }

  def parseResult(result: InvokeResult) = decodeByteBuffer[Route](result.getPayload)
  def parseRouteFromJson(json: Json): Result[Route] = json.as[Route]

  def requestWithPayload(payload: String)(implicit executionContext: ExecutionContext): Future[Json]

}

object AWSLambdaSuggestRouteServiceProxy extends BeelineSuggestRouteService {
  import io.circe.syntax._
  override def requestWithPayload(payload: String)
                                 (implicit executionContext: ExecutionContext): Future[Json] = {
    val lambda = AWSLambdaAsyncClientBuilder.defaultClient()

    val invokeRequest = new InvokeRequest()
      .withFunctionName("beeline-routing-suggestions")
      .withPayload(payload)

    Future {
      // lambda.invokeAsync(invokeRequest).get.getPayload
      val payload = lambda.invokeAsync(invokeRequest).get
      val parsedPayload = parseResult(payload)

      parsedPayload match {
        case Right(r) => r.asJson
        case Left(exc) => throw exc
      }
    }
  }
}

