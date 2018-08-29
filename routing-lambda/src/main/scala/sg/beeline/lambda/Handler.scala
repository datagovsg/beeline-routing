package sg.beeline.lambda

import java.util.concurrent.ForkJoinPool

import sg.beeline.problem._
import com.amazonaws.services.lambda.runtime.Context
import io.circe.Decoder.Result
import io.circe.{Decoder, HCursor}
import io.github.mkotsur.aws.handler.{CanDecode, CanEncode, Lambda}
import sg.beeline.io.Import
import sg.beeline.ruinrecreate.{BeelineRecreateSettings, BeelineSuggestRoute, BeelineSuggestRouteSerdes, SettingsDependentDecoders}

import scala.util.Try
import sg.beeline.ruinrecreate.AWSLambdaSuggestRouteServiceProxy._
import sg.beeline.ruinrecreate.BeelineSuggestRouteService.SuggestRouteInput

import scala.concurrent.ExecutionContext

package object common {
  import io.github.mkotsur.aws.handler.Lambda.{canDecodeAll, canEncodeAll}

  implicit val suggestRouteInputDecoder = BeelineSuggestRouteSerdes.suggestRouteInputDecoder
  implicit val routeEncoder = BeelineSuggestRouteSerdes.routeEncoder

  val canDecodeSuggestRoute = implicitly[CanDecode[SuggestRouteInput]]
  val canEncodeSuggestRoute = implicitly[CanEncode[Route]]
}

object SuggestRouteHandler extends Lambda[SuggestRouteInput, Route]()(common.canDecodeSuggestRoute,
  common.canEncodeSuggestRoute) {

  override def handle(inp: SuggestRouteInput, context: Context)
  : Either[Throwable, Route] = {

    implicit val executionContext = ExecutionContext.fromExecutor(
      new ForkJoinPool(Runtime.getRuntime.availableProcessors))

    val problem = new BasicRoutingProblem(
      settings = inp.settings,
      dataSource = Import,
      suggestions = List()
    )

    val suggestRoute = new BeelineSuggestRoute(
      problem,
      inp.requests, // substitute this with suggestions
      settings = inp.settings
    )

    val feasibleTop50Routes: Either[Throwable, Route] = Try {
      suggestRoute.growRoute(inp.seedRequest, (inp.od.origin, inp.od.destination), inp.requests)
    }.toEither

    feasibleTop50Routes

  }
}
