import sg.beeline._
import sg.beeline.util.Geo

/** Check whether the heading penalty affects travel time */
object HPTest {
  def swap(a : Double, b: Double) = (b,a)
  val pt1 = (103.85253591654006,1.29684825487647)
  val pt2 = swap(1.28984045728,103.834115267)

  Geo.penalizedTravelTime(
    pt1,
    45,
    pt2,
    100
  ) / 60000
  Geo.penalizedTravelTime(
    pt1,
    45,
    pt2,
    280
  ) / 60000

  Geo.travelPathStr(
    pt1,
    45,
    pt2,
    100
  )
  Geo.travelPathStr(
    pt1,
    45,
    pt2,
    280
  )
}