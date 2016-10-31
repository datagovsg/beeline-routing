package sg.beeline

abstract class Activity {
  def minTime : Double
  def maxTime : Double
  def dwellTime : Double
  def serviceTime : Double
  def location : Option[BusStop]
}

case class Pickup(request : Request, _location: BusStop) extends Activity {
  /* say, one hour for pickup */
  def minTime = request.time - 1*3600000
  def maxTime = request.time
  def dwellTime = 60000
  def serviceTime = 0
  def location = Some(_location)
}

case class Dropoff(request : Request, _location: BusStop) extends Activity {
  def minTime = request.time - 15 * 60000
  def maxTime = request.time
  def dwellTime = 60000
  def serviceTime = 0
  def location = Some(_location)
}

case class StartActivity() extends Activity {
  def minTime = 0.0
  def maxTime = Double.PositiveInfinity
  def dwellTime = 0
  def serviceTime = 0
  def location = None
}

case class EndActivity() extends Activity {
  def minTime = 0.0
  def maxTime = Double.PositiveInfinity
  def dwellTime = 0
  def serviceTime = 0
  def location = None
}
