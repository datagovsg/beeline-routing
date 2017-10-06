package sg.beeline.clustering

import FuzzyPointMap.Point
import FuzzyPointMap.withinRange

/**
  * A Fuzzy weighted point map snaps arbitrary points to their
  * nearest grid point, and puts these points in a multimap.
  *
  * The rationale behind this is that we had in the order of 20,000
  * suggestions on Beeline. To do OD clustering, we needed to compute
  * the distance matrix, but a full distance matrix would be unmanageably
  * large and the cluster computation unfeasibly slow (billions of computations).
  *
  * To side-step this problem, we only look at grid points around the cluster
  * centre. e.g. for a 20x20 grid, this will require inspecting a maximum of 400
  * entries.
  *
  * The downside is that up to 10% of points may be put in the wrong cluster
  * centre. Nevertheless, the clustering algorithm is only a heuristic, so a couple
  * of misclassified points is acceptable as long as we can get a full sense of
  * places where people come from and where they want to go.
  *
  */
object FuzzyPointMap {
  type Point = (Double, Double)

  def keyFn(gridSize : Double)(v: Double) = Math.round(v / gridSize).toInt

  /**
    * Returns the grid point for a given point
    * @param gridSize
    * @param point
    * @return
    */
  def key(gridSize : Double)(point : Point) : (Int, Int) = point match {
    case (x, y) => (keyFn(gridSize)(x), keyFn(gridSize)(y))
  }
  /** Returns the coordinates of a given grid point */
  def gridCenter(gridSize: Double)(p: (Int, Int)) = (p._1 * gridSize, p._2 * gridSize)

  def fromSeq[A](points : Traversable[((Double, Double), A)], grid: Double) =
    new FuzzyPointMap[A](
      points.groupBy(x => key(grid)(x._1))
        .mapValues(_.toSet),
      grid)

  def withinRange(point1 : Point, distance: Double, point2: Point) = {
    val dx = (point2._1 - point1._1)
    val dy = (point2._2 - point1._2)

    dx*dx + dy*dy <= distance * distance
  }
}

/**
  * A container class for the FuzzyWeightedPointMap
  *
  * @param map Mapping from grid point to list of weighted points.
  * @param gridSize Resolution at which to store the grid points
  * @tparam A Additional data to be stored with each point
  */
case class FuzzyPointMap[A](map: Map[(Int, Int), Set[((Double, Double), A)]],
                            gridSize: Double) {
  type MapType = Map[(Int, Int), Set[((Double, Double), A)]]

  def cells(point : Point, r: Double) = {
    val minX = FuzzyPointMap.keyFn(gridSize)(point._1 - r)
    val maxX = FuzzyPointMap.keyFn(gridSize)(point._1 + r)
    val minY = FuzzyPointMap.keyFn(gridSize)(point._2 - r)
    val maxY = FuzzyPointMap.keyFn(gridSize)(point._2 + r)

    def allIncluded(key: (Int, Int)) = key match {
      case (kx, ky) =>
        withinRange(point, r, (kx*gridSize,ky*gridSize))
    }

    val candidateKeys = for (x <- minX to maxX; y <- minY to maxY) yield(x,y)

    candidateKeys.filter(x => map.contains(x)).filter(allIncluded)
  }

  def countRange(point: Point, r: Double) = {
    val fullCells = this.cells(point, r)

    fullCells.view.map({k => map.get(k) match {
      case Some(set) => set.map(_._2).size
      case None => 0
    }}).sum
  }

  def queryRange(point: Point, r: Double) = {
    val fullCells = this.cells(point, r)

    fullCells.view.flatMap({k => map(k)})
  }

  def updated(map : MapType) =
    FuzzyPointMap(map, gridSize)
}