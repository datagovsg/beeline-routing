package  sg.beeline
/**
  * Created by daniel on 12/12/16.
  */
class SuggestParameters(
                       // Max radius around search point to search for
                       // convenient stops
                       originClusterRadius : Double,
                       destinationClusterRadius : Double,

                       // Max count of stops
                       originClusterLimit: Option[Int],
                       destinationClusterLimit: Option[Int]
                       ) {

}
