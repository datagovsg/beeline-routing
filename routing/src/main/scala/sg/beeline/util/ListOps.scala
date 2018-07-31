package sg.beeline.util

import scala.annotation.tailrec

class ListOps[A](t: Traversable[A]) {
  // FIXME: fix the tail recursion bit
  def groupSuccessive[B](fn : A => B) : List[(B, Traversable[A])] = {

    @tailrec
    def step(t: Traversable[A],
             acc: List[(B, Traversable[A])]): List[(B, Traversable[A])] = {
      if (t.isEmpty) {
        acc
      } else {
        val first = fn(t.head)
        val (same, different) = t.span(x => fn(x) == first)

        step(
          different,
          (first, same) :: acc
        )
      }
    }

    step(t, List()).reverse
  }
}
