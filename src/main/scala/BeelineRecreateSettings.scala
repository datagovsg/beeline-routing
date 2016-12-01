package sg.beeline

/**
  * Created by daniel on 25/11/16.
  */
class BeelineRecreateSettings(
                             val maxDetourMinutes : Double = 2.0,
                             val clusterRadius : Int = 4000
                             ) {

}

object BeelineRecreateSettings {
  val default =
    new BeelineRecreateSettings(2.0, 4000)
}