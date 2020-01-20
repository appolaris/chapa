package chapa

import scala.annotation.tailrec

object Purger {
  /**
    * Removes less complete edges (parse trees) where more complete alternatives exist.
    */
  def purge(rootSymbol: Symbol[_], edges: Set[Edge]): List[Edge] = {
    val ss = edges.filter(_.symbol == rootSymbol).toList
    filter(ss, Nil)
  }

  @tailrec
  private def filter(toCheck: List[Edge], res: List[Edge]): List[Edge] = toCheck match {
    case h::tail =>
      // if unprocessed or already found edges do not contain larger or equal edge
      if (!(tail ++ res).exists(t => compareEdges(t, h).exists(x => x >= 0)))
        filter(tail, h::res)
      else
        filter(tail, res)
    case _ => res
  }

  /**
    * Compares two edges. EdgeA is smaller than EdgeB if:
    * 1) EdgeA and EdgeB trees are isomorphic.
    * 2) For each non-empty node in EdgeB tree there is a corresponding non-empty node in EdgeA tree.
    * 3) But at least one non-empty node in EdgeA tree has corresponding empty node in EdgeA tree.
    * @return Some(-1) if left < right
    *         Some(+1) if left > right
    *         Some(0) if left == right
    *         None if edges are incomparable.
    */
  def compareEdges(left: Edge, right: Edge): Option[Int] = {
    if (left.symbol != right.symbol) return None
    (left, right) match {
      case (_: TerminalEdge, _: TerminalEdge) => Some(0)
      case (left: RuleEdge, right: RuleEdge) =>
        if (left.rule != right.rule) return None
        (left.children zip right.children).map {
          case (None, None) => Some(0)
          case (Some(_), None) => Some(+1)
          case (None, Some(_)) => Some(-1)
          case (Some(l), Some(r)) => compareEdges(l, r)
        }.fold(Some(0) /* starting assuming that edges are equal */) {
          case (None, _) | (_, None) => None
          case (Some(0), Some(x)) => Some(x)
          case (Some(x), Some(0)) => Some(x)
          case (Some(x), Some(y)) if x == y => Some(x)
          case _ => None
        }
      case _ => None
    }
  }
}
