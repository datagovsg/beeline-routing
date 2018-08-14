package sg.beeline.problem

sealed trait Activity {
  def minTime : Double
  def maxTime : Double
  def dwellTime : Double
  def serviceTime : Double
  def location : Option[BusStop]
}

case class Pickup(request : Request, _location: BusStop) extends Activity {
  /* say, one hour for pickup */
  override val minTime = request.time - 2*3600000
  override val maxTime = request.time
  override val dwellTime = 60000
  override val serviceTime = 0
  override val location = Some(_location)
}

case class Dropoff(request : Request, _location: BusStop) extends Activity {
  override val minTime = request.time - 15 * 60000
  override val maxTime = request.time
  override val dwellTime = 60000
  override val serviceTime = 0
  override val location = Some(_location)
}

case class StartActivity() extends Activity {
  override val minTime = 0.0
  override val maxTime = Double.PositiveInfinity
  override val dwellTime = 0
  override val serviceTime = 0
  override val location = None
}

case class EndActivity() extends Activity {
  override val minTime = 0.0
  override val maxTime = Double.PositiveInfinity
  override val dwellTime = 0
  override val serviceTime = 0
  override val location = None
}
