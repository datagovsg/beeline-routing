package sg.beeline

import com.thesamet.spatial.{KDTreeMap, RegionBuilder}

import scala.math.{Pi, asin, cos, sin, sqrt}

package object util {

  type Point = (Double, Double)

  /*
    Calculate distance between two points on Earth (in metres)
   */
  def computeDistance(point1 : Point, point2 : Point) : Double = {
    val lon1 = toRadians(point1._1)
    val lat1 = toRadians(point1._2)
    val lon2 = toRadians(point2._1)
    val lat2 = toRadians(point2._2)
    // println(s"${lon1} ${lat1} ${lon2} ${lat2}")

    val dlon = lon2 - lon1
    val dlat = lat2 - lat1
    val sinDlat = sin(dlat/2)
    val sinDlon = sin(dlon/2)
    val a = sinDlat * sinDlat + cos(lat1) * cos(lat2) * sinDlon * sinDlon
    val angle = 2 * asin(sqrt(a))
    6367 * 1000 * angle
  }

  def toRadians(degree: Double) : Double = {
    Pi * degree / 180
  }

  def squaredDistance(a: Point, b: Point) =
    (a._2 - b._2)*(a._2 - b._2) +
      (a._1 - b._1)*(a._1 - b._1)

  object KDTreeQuery {

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

}
