package sg.beeline.util

/**
  * Created by daniel on 14/12/16.
  */

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.Random
;

object WeightedRandomShuffle {

  def shuffle[A <: AnyRef](items : Traversable[A], weights : Traversable[Double])
                (implicit c : ClassTag[A]): Seq[A]= {
    if (items.isEmpty) {
      Array[A]()
    } else {
      val arrayed = weights.toArray
      val arrayedItems = items.toArray[A]

      // max array length is 2^31-1
      val root = buildTree(arrayed, 29)

      @tailrec
      def nextIteration(
                         acc : mutable.ArrayBuilder[A],
                         cache : TreeSumNode,
                         count : Int
                       ) : mutable.ArrayBuilder[A] = {
        if (count == 0) {
          acc
        } else {
          val rand = Random.nextDouble * cache.sum
          val pickIndex = cache.at(rand)

          acc += arrayedItems(pickIndex)

          nextIteration(
            acc,
            cache.updated(pickIndex, 0.0),
            count - 1)
        }
      }
      nextIteration(new mutable.ArrayBuilder.ofRef[A], root, root.maxIndex + 1)
        .result()
    }
  }

  def buildTree(array : Array[Double], logMid : Int, offset : Int = 0)
  : TreeSumNode = {
    require(array.size <= (1 << 30))
    require(0 <= logMid && logMid <= 30)

    val size = 1 << logMid
    if (size == 1)
      TreeSumLeaf(
        if (offset < array.size) array(offset)
        else 0,
        offset
      )
    else if (offset + size / 2 < array.size)
    // Oops can make this more efficient but never mind
      TreeSumBranch(
        buildTree(array, logMid - 1, offset),
        buildTree(array, logMid - 1, offset + size / 2)
      )
    else
      buildTree(array, logMid - 1, offset)
  }

  abstract class TreeSumNode {
    def sum : Double
    def minIndex : Int
    def maxIndex : Int

    def sumRange(lower : Int, upper : Int) : Double
    def updated(index : Int, weight : Double) : TreeSumNode
    def at(w : Double) : Int

    def walk : IndexedSeq[Double]
  }
  case class TreeSumBranch(
                          left : TreeSumNode,
                          right : TreeSumNode
                          ) extends TreeSumNode {
    val minIndex = left.minIndex
    val maxIndex = right.maxIndex
    val sum = left.sum + right.sum

    def sumRange(lower : Int, upper : Int) = {
      require(lower >= minIndex)
      require(upper <= maxIndex + 1)

      if (lower == minIndex && upper == maxIndex + 1)
        sum
      else if (lower >= upper)
        0
      else
        left.sumRange(
          Math.max(left.minIndex, lower),
          Math.min(left.maxIndex + 1, upper)) +
        right.sumRange(
          Math.max(right.minIndex, lower),
          Math.min(right.maxIndex + 1, upper)
        )
    }

    def updated(index : Int, weight : Double) : TreeSumNode = {
      if (left.minIndex <= index && index <= left.maxIndex)
        TreeSumBranch(
          left.updated(index, weight),
          right
        )
      else if (right.minIndex <= index && index <= right.maxIndex)
        TreeSumBranch(
          left,
          right.updated(index, weight)
        )
      else
        throw new IllegalArgumentException("Neither in left or right range")
    }

    def at(w : Double) : Int = {
      if (left.sum == 0.0)
        right.at(w)
      else if (right.sum == 0.0)
        left.at(w)
      else if (w > left.sum)
        right.at(w - left.sum)
      else
        left.at(w)
    }

    def walk : IndexedSeq[Double] =
      left.walk ++ right.walk
  }

  case class TreeSumLeaf(
                        value : Double,
                        index : Int
                        ) extends TreeSumNode {
    val minIndex = index
    val maxIndex = index
    val sum = value

    def sumRange(lower: Int, upper: Int) = {
      if (lower >= upper)
        0
      else {
        require(lower == index)
        require(upper == index + 1)
        value
      }
    }

    def updated(index : Int, weight : Double) = {
      require(index == this.index)
      TreeSumLeaf(weight, index)
    }

    def at(w : Double) = {
      index
    }

    def walk = IndexedSeq(sum)
  }
}
