package sg.beeline.util

import com.vividsolutions.jts.geom.{Coordinate, GeometryFactory}
import io.jeo.proj.Proj
import org.osgeo.proj4j.CoordinateReferenceSystem

object Projections {
  val geometryFactory = new GeometryFactory()
  val svy21 = Proj.crs("epsg:3414")
  val wgs = Proj.`EPSG_4326`

  def reproject(from: CoordinateReferenceSystem, to: CoordinateReferenceSystem)(point: Point): Point = {
    val p = geometryFactory.createPoint(new Coordinate(point._1, point._2))
    val reprojected = Proj.reproject(p, from, to)
    val reprojectedCoordinate = reprojected.getCoordinate
    (reprojectedCoordinate.x, reprojectedCoordinate.y)
  }

  def toSVY: Point => Point = reproject(wgs, svy21)
  def toWGS: Point => Point = reproject(svy21, wgs)

}
