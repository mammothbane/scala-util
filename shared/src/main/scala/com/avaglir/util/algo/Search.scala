package com.avaglir.util.algo

import scala.annotation.tailrec

object Search {
  // found here: https://github.com/raboof/astar/blob/master/src/main/scala/Astar.scala
  def aStar[S, @specialized(Specializable.AllNumeric) T: Ordering](init: S, expand: S => Set[S], heuristic: S => T, value: S => T): S = {
    val ord = implicitly[Ordering[T]]
    import ord._

    @tailrec
    def search(unexpanded: Set[S], best: S, seen: Set[S]): S =
      unexpanded.toList.map(s => (s, heuristic(s))).sortBy{ case (_, heur) => heur }(ord.reverse).headOption match {
        case None => best
        case Some((_, heur)) if heur <= value(best) => best
        case Some((exp, _)) =>
          val expanded = expand(exp) diff seen
          val result = if (value(exp) > value(best)) exp else best
          val bestV = value(result)
          search(
            (unexpanded.filter { _ != exp } ++ expanded).filter { heuristic(_) > bestV },
            result,
            (seen + exp).filter { heuristic(_) > bestV }
          )
      }

    search(Set(init), init, Set.empty)
  }

  def bfs[S](init: S, expand: S => List[S]): Set[S] = {
    @tailrec
    def search(unexpanded: List[S], seen: Set[S]): Set[S] = unexpanded match {
      case Nil => seen
      case elem :: tail if seen contains elem => search(tail, seen)
      case elem :: tail => search(expand(elem) ++ tail, seen + elem)
    }

    search(List(init), Set.empty)
  }
}
