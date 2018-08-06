package sg.beeline.io

import sg.beeline.problem.BusStop

import scala.io.Source

object BuiltIn extends DataSource {
  override def distanceFunction(a: BusStop, b: BusStop) =
    distanceMatrix(a.index)(b.index)

  override lazy val busStops = {
    implicit val busStopSchemaDecoder = _root_.io.circe.generic
      .semiauto.deriveDecoder[BusStopSchema]

    val jsonData = _root_.io.circe.parser.decode[List[BusStopSchema]](
      Source.fromInputStream(
        this.getClass.getResourceAsStream("/bus-stops-headings.json")
      ).mkString
    ).right.get

    jsonData
      .zipWithIndex
      .filter(s => s._1.Longitude != 0 && s._1.Latitude != 0)
      .map({
        case (b, i) => BusStop(
          (b.Longitude, b.Latitude),
          b.Heading.getOrElse(Double.NaN),
          b.Description,
          b.RoadName,
          i
        )
      })
  }

  lazy val distanceMatrix = {
    val ois = new java.io.ObjectInputStream(
      new java.util.zip.GZIPInputStream(
        this.getClass.getResourceAsStream("/distances_cache.dat.gz")))

    /* FIXME Hack: Slow down all timings by 50% to account for peak
      hour bad traffic
     */
    val arr = ois.readObject().asInstanceOf[Array[Array[Double]]]
    arr.foreach { row =>
      row.indices.foreach { i =>
        row(i) = row(i) * 1.5
      }
    }

    ois.close()
    arr
  }

  // Return the number of seconds since midnight
  def convertTime(timeString: String) =
    timeString.substring(0,2).toLong * 3600000 +
      timeString.substring(2,4).toLong * 60000

  def getLiveRequests = throw new UnsupportedOperationException

}
