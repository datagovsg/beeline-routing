package sg.beeline.clustering

import scala.annotation.tailrec
import scalaz._
import Scalaz._

class FuzzyDistanceCluster[A](distance: Double, gridSize: Double)
  extends Function1[Seq[((Double, Double), A)],
                    Map[(Double, Double), Seq[((Double, Double), A)]]] {

  override def apply(v1: Seq[((Double, Double), A)]) = {
    val fuzzyPointMap = FuzzyPointMap.fromSeq(v1, gridSize)

    val neighbourCounts = fuzzyPointMap.map.map({
      case (key, _) =>
        val center = FuzzyPointMap.gridCenter(gridSize)(key)
        val cellsInRange = fuzzyPointMap.cells(center, distance)
        val numNeighbours = cellsInRange.map(uv => fuzzyPointMap.map(uv).size).sum

        (key, numNeighbours)
    })

    // We want to pick off entries one-by-one by the max number
    // of unselected neighbours. We use a priority queue to count
    // the number of unselected numbers.
    val priorityQueue = {
      val initialNeighbourCount = v1.zipWithIndex.map({
        case ((xy, payload), index) =>
          val uv = FuzzyPointMap.key(gridSize)(xy)
          (
            neighbourCounts(uv),
            uv
          )
      }).toSet

      scala.collection.mutable.PriorityQueue[(Int, (Int, Int))]() ++ initialNeighbourCount
    }

    @tailrec
    def nextRepresentative(acc : List[((Double, Double), Seq[((Double, Double), A)])],
                           unpickedSet : Set[(Int, Int)],
                           fuzzyPointMap: FuzzyPointMap[A]) : List[((Double, Double), Seq[((Double, Double), A)])] = {
      if (priorityQueue.isEmpty)
        acc
      else {
        val (score, uv) = priorityQueue.dequeue()
        lazy val xy = FuzzyPointMap.gridCenter(gridSize)(uv)
        lazy val neighbouringGridCentres = fuzzyPointMap.cells(xy, distance)
        lazy val actualScore = neighbouringGridCentres.map(c => fuzzyPointMap.map(c).size).sum

        if (!unpickedSet.contains(uv)) // this centre has been picked before
          nextRepresentative(acc, unpickedSet, fuzzyPointMap)
        else if (actualScore == score) { // this centre has not been picked, and the count is up to date
          nextRepresentative(
            (xy, neighbouringGridCentres.flatMap(c => fuzzyPointMap.map(c))) :: acc,
            unpickedSet -- neighbouringGridCentres,
            fuzzyPointMap.updated(fuzzyPointMap.map -- neighbouringGridCentres)
          )
        }
        else if (actualScore < score) {
          if (actualScore > 0) {
            priorityQueue += ((actualScore, uv))
          }
          nextRepresentative(acc, unpickedSet, fuzzyPointMap)
        }
        else
          throw new IllegalStateException()
      }
    }

    nextRepresentative(
      List(),
      fuzzyPointMap.map.keySet,
      fuzzyPointMap
    ).toMap
  }
}
