package sg.beeline.ruinrecreate

case class BeelineRecreateSettings(
  maxDetourMinutes : Double = 15.0,
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
)

object BeelineRecreateSettings {
  lazy val DEFAULT = new BeelineRecreateSettings()
}
