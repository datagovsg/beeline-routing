package sg.beeline.ui

import sg.beeline.Util

/**
  * Created by daniel on 26/7/17.
  */
abstract class Region {
  def contains(latLng : (Double, Double)) : Boolean
}

case class CircularRegion(val lngLat : (Double, Double), val radiusInMetres : Double) extends Region {
  val xy = Util.toSVY(lngLat)

  def contains(xy2 : (Double, Double)) = {
    // val xy2 = Util.toSVY(latLng)

    val (x1,y1) = xy
    val (x2,y2) = xy2

    (x2 - x1)*(x2-x1) + (y2-y1)*(y2-y1) <= radiusInMetres * radiusInMetres
  }

  override def toString = s"${lngLat._2},${lngLat._1} <--> ${radiusInMetres}m"
}
