package sg.beeline.util

import scala.annotation.tailrec

class ListInsertionPointOps[T](list: List[T]) {

  /**
    * After inserting a1 at ip1, and a2 at ip2
    * return ip1._1, a1, ip1._2, ... ip2._1, a2, ip2._2
    *
    * If ip1 == ip2, then return ip1._1, a1, a2, ip2._2
    *
    * @param a1
    * @param a2
    * @param ip1
    * @param ip2
    * @return
    */
  def insertedSubsequence(a1: T,
                          a2: T,
                          ip1 : (T, T),
                          ip2: (T, T)): List[T] = {
    if (ip1 == ip2) {
      require(list.sliding(2).contains(List(ip1._1, ip1._2)))
      List(ip1._1, a1, a2, ip1._2)
    }
    else {
      sealed trait FindState
      case class NothingFound() extends FindState
      case class FirstInsertionFound() extends FindState
      case class SecondInsertionFound() extends FindState

      type InsertionState = (T, (T, T))
      val listBuilder = List.newBuilder[T]

      def step(insertionState: FindState, xs: List[T]): List[T] = insertionState match {
        case NothingFound() =>
          xs match {
            case x :: y :: tail =>
              if ((x, y) == ip1) {
                listBuilder += x
                listBuilder += a1
                step(FirstInsertionFound(), y :: tail)
              } else {
                step(insertionState, y :: tail)
              }
            case _ => throw new IllegalArgumentException("Reached end of list, but insertion point not found")
          }
        case FirstInsertionFound() =>
          xs match {
            case x :: y :: tail =>
              if ((x, y) == ip2) {
                listBuilder += x
                listBuilder += a2
                listBuilder += y
                step(SecondInsertionFound(), y :: tail)
              } else {
                listBuilder += x
                step(FirstInsertionFound(), y :: tail)
              }
            case _ => throw new IllegalArgumentException("Reached end of list, but insertion point not found")
          }
        case SecondInsertionFound() => listBuilder.result()
      }

      step(
        NothingFound(),
        list
      )
    }
  }

  def insert(a1: T,
             a2: T,
             ip1 : (T, T),
             ip2: (T, T)): List[T] = {

    type InsertionState = (List[T], (T, T))
    val listBuilder = List.newBuilder[T]

    @tailrec
    def step(insertionState: List[InsertionState], xs: List[T]): List[T] = insertionState match {
      case (items, insertionPoint) :: remainingInsertions =>
        xs match {
          case x :: y :: tail =>
            if ((x, y) == insertionPoint) {
              listBuilder += x
              listBuilder ++= items
              step(remainingInsertions, y :: tail)
            } else {
              listBuilder += x
              step(insertionState, y :: tail)
            }
          case _ => throw new IllegalArgumentException("Reached end of list, but insertion point not found")
        }
      case Nil =>
        listBuilder ++= xs
        listBuilder.result()
    }

    if (ip1 == ip2) {
      step(
        List( (List(a1, a2), ip1) ),
        list
      )
    }
    else {
      step(
        List( (List(a1), ip1), (List(a2), ip2) ),
        list
      )
    }
  }
}
