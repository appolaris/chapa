package chapa

import scala.annotation.tailrec

object ChartParser {
  def parse(chartRules: (Grammar, Edge, Set[Edge]) => List[Edge], grammar: Grammar, initialAgenda: Seq[Edge]): Set[Edge] = {
    val initialChart = Set[Edge]()
    val chart = step(grammar, chartRules, initialAgenda, initialChart)
    // todo: group to ASTs
    chart
  }

  @tailrec
  private final def step(grammar: Grammar,
                         chartRules: (Grammar, Edge, Set[Edge]) => List[Edge],
                         agenda: Seq[Edge],
                         chart: Set[Edge]): Set[Edge] = agenda match {
    case Nil => chart
    case edge::edges =>
      val newEdges = chartRules(grammar, edge, chart)
      val newAgenda = edges ++ newEdges // TODO: dfs vs bfs ?
      val newChart = chart + edge
      step(grammar, chartRules, newAgenda, newChart)
  }
}
