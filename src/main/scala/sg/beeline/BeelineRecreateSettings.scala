package sg.beeline

/**
  * Created by daniel on 25/11/16.
  */
case class BeelineRecreateSettings(
                                   maxDetourMinutes : Double = 2.0,
                                   startClusterRadius : Int = 4000,
                                   startWalkingDistance : Int = 400,
                                   endClusterRadius : Int = 4000,
                                   endWalkingDistance : Int = 400,
                                   dataSource : String = "suggestions"
                             ) {
}

object BeelineRecreateSettings {
  lazy val default = new BeelineRecreateSettings()
}
