package sg.beeline
import io.jeo.proj.Proj
import com.vividsolutions.jts.geom.{GeometryFactory, PrecisionModel, Coordinate}
import com.thesamet.spatial.{KDTreeMap, RegionBuilder}

object Util {
  type Point = (Double, Double)
  val geometryFactory = new GeometryFactory()
  val wgs = Proj.`EPSG_4326`
  val svy21 = Proj.crs("epsg:3414")

  def toSVY(point: Point) = {
    val reprojected = Proj.reproject(
      geometryFactory.createPoint(new Coordinate(point._1, point._2)),
      wgs,
      svy21
    )
    val reprojectedCoordinate = reprojected.getCoordinate
    (reprojectedCoordinate.x, reprojectedCoordinate.y)
  }
  def toWGS(point: Point) = {
    val reprojected = Proj.reproject(
      geometryFactory.createPoint(new Coordinate(point._1, point._2)),
      svy21,
      wgs
    )
    val reprojectedCoordinate = reprojected.getCoordinate
    (reprojectedCoordinate.x, reprojectedCoordinate.y)
  }
}

object kdtreeQuery {
  def squaredDistance(a: (Double, Double), b: (Double, Double)) =
    (a._2 - b._2)*(a._2 - b._2) +
    (a._1 - b._1)*(a._1 - b._1)

  def queryBall[O](
    treeMap: KDTreeMap[(Double, Double), O],
    origin: (Double, Double),
    distance: Double)
      : Seq[((Double,Double),O)] = {
    val region = new RegionBuilder[(Double, Double)] {
      from  ((origin._1 - distance, 0), 0)
      to    ((origin._1 + distance, 0), 0)
      from  ((0, origin._2 - distance), 1)
      to    ((0, origin._2 + distance), 1)
    }.build

    treeMap.regionQuery(region) filter {
      (pair) => squaredDistance(pair._1, origin) <= distance * distance
    }
  }

  implicit class KDTreeMapBall[B](treeMap: KDTreeMap[(Double, Double),B]) {

    def queryBall(origin: (Double, Double),
                      distance: Double)
    : Seq[((Double, Double),B)] = {
      val region = new RegionBuilder[(Double, Double)] {
        from  ((origin._1 - distance, 0), 0)
        to    ((origin._1 + distance, 0), 0)
        from  ((0, origin._2 - distance), 1)
        to    ((0, origin._2 + distance), 1)
      }.build

      treeMap.regionQuery(region) filter {
        (pair) => squaredDistance(pair._1, origin) <= distance * distance
      }
    }
  }
}