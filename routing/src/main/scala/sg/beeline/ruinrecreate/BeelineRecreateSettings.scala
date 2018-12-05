package sg.beeline.ruinrecreate

import sg.beeline.problem.Suggestion
import sg.beeline.util.squaredDistance

case class BeelineRecreateSettings(maxDetourMinutes : Double = 15.0,
                                   startClusterRadius : Int = 4000,
                                   startWalkingDistance : Int = 400,
                                   endClusterRadius : Int = 4000,
                                   endWalkingDistance : Int = 400,

                                   timeAllowance: Long = 1800 * 1000L, // Half an hour
                                   similarityLimit: Float = 0.3f,
                                   imputedDwellTime: Long = 60000,
                                   suboptimalStopChoiceAllowance: Long = 60000,

                                   includeAnonymous: Boolean = true,
                                   matchDaysOfWeek: Boolean = true,
                                   createdSince: Long = 0L,
                                   minRequests: Int = 15,

                                   dataSource : String = "suggestions"
                             ) {

  def suggestionsFilter(reference: Suggestion): Suggestion => Boolean = { s: Suggestion =>
    // Determine whether or not to allow anonymous suggestions
    (includeAnonymous || s.userId.nonEmpty || s.email.nonEmpty) &&
      // Min created time (to ignore the really old requests)
      s.createdAt > createdSince &&
      // Ensure arrival time is plus/minus some value
      (s.time - reference.time).abs <= timeAllowance &&
      // Ensure that the daysOfWeek mask overlaps to some extent
      (!matchDaysOfWeek || (s.daysOfWeek & reference.daysOfWeek) != 0) &&
      squaredDistance(reference.start, s.start) < startClusterRadius * startClusterRadius &&
      squaredDistance(reference.end, s.end) < endClusterRadius * endClusterRadius
  }
}

object BeelineRecreateSettings {
  lazy val DEFAULT = new BeelineRecreateSettings()
}
