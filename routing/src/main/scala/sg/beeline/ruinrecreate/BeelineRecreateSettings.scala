package sg.beeline.ruinrecreate

import com.thesamet.spatial.KDTreeMap
import sg.beeline.problem.Request
import sg.beeline.util.Util.Point
import sg.beeline.util.kdtreeQuery

case class BeelineRecreateSettings(
                                   maxDetourMinutes : Double = 2.0,
                                   startClusterRadius : Int = 4000,
                                   startWalkingDistance : Int = 400,
                                   endClusterRadius : Int = 4000,
                                   endWalkingDistance : Int = 400,

                                   timeAllowance: Long = 1800 * 1000L, // Half an hour
                                   daysOfWeek: Int = 31, // 0b0011111 = Mon-Fri

                                   dataSource : String = "suggestions"
                             ) {

  def requestsFilter(seedRequest: Request): Request => Boolean = {
    (r: Request) =>
      (r.time - seedRequest.time).abs <= timeAllowance &&
        kdtreeQuery.squaredDistance(seedRequest.start, r.start) < startClusterRadius * startClusterRadius &&
        kdtreeQuery.squaredDistance(seedRequest.end, r.end) < endClusterRadius * endClusterRadius
  }
}

object BeelineRecreateSettings {
  lazy val default = new BeelineRecreateSettings()
}
