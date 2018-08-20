package sg.beeline.lambda

import java.util.concurrent.ForkJoinPool

import sg.beeline.problem._
import com.amazonaws.services.lambda.runtime.Context
import io.github.mkotsur.aws.handler.{CanDecode, CanEncode, Lambda}
import io.github.mkotsur.aws.handler.Lambda._
import sg.beeline.io.Import
import sg.beeline.lambda.common.problem
import sg.beeline.ruinrecreate.{BeelineRecreateSettings, BeelineSuggestRoute}

import scala.util.Try
import sg.beeline.ruinrecreate.AWSLambdaSuggestRouteServiceProxy._
import sg.beeline.ruinrecreate.BeelineSuggestRouteService.SuggestRouteInput

import scala.concurrent.ExecutionContext

package object common {
  import io.github.mkotsur.aws.handler.Lambda.{canDecodeAll, canEncodeAll}

  val problem = new BasicRoutingProblem(
    List(),
    // TODO: get from settings / env var
    300.0, // ...getOrElse(300.0)
    300.0,
    dataSource = Import
  )

  val canDecodeSuggestRoute = implicitly[CanDecode[SuggestRouteInput]]
  val canEncodeSuggestRoute = implicitly[CanEncode[Route]]
}

object SuggestRouteHandler extends Lambda[SuggestRouteInput, Route]()(common.canDecodeSuggestRoute,
  common.canEncodeSuggestRoute) {

  override def handle(inp: SuggestRouteInput, context: Context)
  : Either[Throwable, Route] = {

    implicit val executionContext = ExecutionContext.fromExecutor(
      new ForkJoinPool(Runtime.getRuntime.availableProcessors))

    val suggestRoute = new BeelineSuggestRoute(
      problem,
      inp.requests, // substitute this with suggestions
      settings = BeelineRecreateSettings.default
    )

    val feasibleTop50Routes: Either[Throwable, Route] = Try {
      suggestRoute.growRoute(inp.seedRequest, (inp.od.origin, inp.od.destination), inp.requests)
    }.toEither

    feasibleTop50Routes

  }
}
