package sg.beeline.lambda

case class SuggestRouteInput(
                            seedRequest: Request,
                            requests: List[Request],
                            )
case class SuggestRouteOutput(
                             route: Route
                             )

class SuggestRouteHandler extends Lambda[SuggestRouteInput, SuggestRouteOutput] {
  override def handler(inp: SuggestRouteInput, context: Context) = {

  }
}