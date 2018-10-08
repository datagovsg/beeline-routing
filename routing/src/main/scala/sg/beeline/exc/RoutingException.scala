package sg.beeline.exc

abstract class RoutingException(val reason: String, message: String) extends Exception(reason + ", " + message)

class NoRoutesFound(message: String = "") extends RoutingException("no_routes_found", message)

/**
  * If the number of requests served is less than minRequests
  */
class TooFewSuggestions(message: String = "") extends RoutingException("too_few_suggestions", message)

/**
  * If for some reason, the server was unable to match / create new stops on the main API
  */
class FailedToGenerateStops(message: String = "") extends RoutingException("failed_to_generate_stops", message)

class UserNotAuthorized(message: String = "") extends RoutingException("user_not_authorized", message)

/**
  * If the call to the Google Maps API failed
  */
class FailedToEstimateTravelTime(message: String = "") extends RoutingException("failed_to_estimate_travel_time", message)

/**
  * If the call to POST /suggested_routes failed
  */
class FailedToSubmitRoute(message: String = "") extends RoutingException("failed_to_submit_route", message)