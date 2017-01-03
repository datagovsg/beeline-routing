package sg.beeline

/**
  * Created by daniel on 25/11/16.
  */
class BeelineRecreateSettings(
                               val maxDetourMinutes : Double = 2.0,
                               val startClusterRadius : Int = 4000,
                               val startWalkingDistance : Int = 400,
                               val endClusterRadius : Int = 4000,
                               val endWalkingDistance : Int = 400,
                               val dataSource : String = "suggestions"
                             ) {

}

object BeelineRecreateSettings {
  lazy val default = new BeelineRecreateSettings()
}