package chapa

import scala.annotation.tailrec

object Parser2 {
  case class State(agenda: List[Set[Edge]], result: Set[Set[Edge]]) // TODO: better name?

  /** Builds compLete trees (starting from 5) by combining and extending (with "holes") set of inactive edges.
    * @param chart Inactive edges obtained by the normal, bottom-up chart parsing. Only top Level edges are expected.
    * @return Complete trees (starting from 5) obtained by applying rules to combine & extend subtrees.
    */
  def combine(chart: Set[Edge]): Set[Set[Edge]] = {
    combineInt(State(List(chart), Set()))
  }

  @tailrec
  def combineInt(state: State): Set[Set[Edge]] = state.agenda match {
    case chart::tail =>
      val newState = combineAtomic(chart)
      combineInt(State(newState.agenda ++ tail, newState.result ++ state.result))
    case Nil => state.result
  }

  /** Takes incomplete chart (set of edges/subtrees) and tries to make a more complete chart or a final tree rooted at S.
    * How it is done depends on particular rules.
    * @param chart Set of inactive rules to be combined / extended.
    * @return New incomplete charts or final, results (S edges).
    */
  def combineAtomic(chart: Set[Edge]): State = ???
}
