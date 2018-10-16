package sg.beeline

package object exc {
  class RoutingException(val reason: String, message: String) extends Exception(reason + ", " + message)

  class RoutingExceptionBuilder(reason: String)
    extends (() => RoutingException)
    with (String => RoutingException) {

    override def apply(): RoutingException = apply("")

    override def apply(message: String): RoutingException = new RoutingException(reason, message)
  }

  def NoRoutesFound = new RoutingExceptionBuilder("no_routes_found")
  def TooFewSuggestions = new RoutingExceptionBuilder("too_few_suggestions")
  def FailedToGenerateStops = new RoutingExceptionBuilder("failed_to_generate_stops")
  def UserNotAuthorized = new RoutingExceptionBuilder("user_not_authorized")
  def FailedToEstimateTravelTime = new RoutingExceptionBuilder("failed_to_estimate_travel_time")
  def FailedToSubmitRoute = new RoutingExceptionBuilder("failed_to_submit_route")
}
