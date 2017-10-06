package sg.beeline.clustering

import java.util.Scanner

import com.thesamet.spatial.{KDTree, Region}

import scala.annotation.tailrec
import scala.collection.immutable.HashSet

object Cluster {

  def main(args : Array[String]) {
    // Read the input...
    val (distance, requests) = {
      val inp = new Scanner(new java.io.File(
      "C:/Users/Daniel/Desktop/suggestion-clustering/sample_input")
      )
      val distance = inp.nextInt()
      val numEntries = inp.nextInt()
      val locationsArray = scala.collection.mutable.ArrayBuilder.make[(Double, Double)]

      while (inp.hasNext()) {
        locationsArray += ((inp.nextDouble(), inp.nextDouble()))
      }
      (distance, locationsArray.result())
    }

    // Make the kd tree

    type Point = (Double, Double)
    val kdtree = KDTree.fromSeq(requests)

    println("Built KDTree")

    var i = 0

    @tailrec
    def nextRepresentative(
                            acc : List[(Point, Set[Point])],
                            takenSet : Set[Point],
                            neighbourMap: Map[Point, Set[Point]])
    : List[(Point, Set[Point])] = {
      if (i % 1 == 0) {
        println(s"Step ${i}")
      }
      i += 1

      if (neighbourMap.isEmpty)
        acc
      else {
        val updatedNeighbourMap = neighbourMap.mapValues(
          _ -- takenSet
        )

        val max = updatedNeighbourMap.maxBy({
          case (point, neighbours) => neighbours.size
        })

        if (max._2.size == 1)
          updatedNeighbourMap.toList ++ acc
        else
          nextRepresentative(
            (max._1, max._2) :: acc,
            /*takenSet ++ */ max._2,
            updatedNeighbourMap -- takenSet
          )
      }
    }

    def sqDist(p1 : Point, p2 : Point) : Double = (p1, p2) match {
      case ((x,y), (a,b)) =>
        (a - x) * (a - x) + (b-y) * (b-y)
    }

    val initialNeighbourMap = requests.map({
      case (x,y) =>
        val region : Region[Point] = Region
            .from((x - distance, y), 0)
            .to((x + distance, y), 0)
            .from((x, y-distance), 1)
            .to((x, y+distance), 1)
            .build
        ((x,y), kdtree.regionQuery(region)
            .filter({pt => sqDist(pt, (x,y)) <= distance * distance})
            .toSet
        )
    }).toMap
    println("Built Neighbour map")
    println(initialNeighbourMap.values.map(_.size).sum / initialNeighbourMap.size)

    val result = nextRepresentative(List(), new HashSet[Point](), initialNeighbourMap)

    println(result.take(50))
  }
}
