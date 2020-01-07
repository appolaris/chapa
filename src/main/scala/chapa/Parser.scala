package chapa

import scala.annotation.tailrec

case class ParserState(agenda: List[Edge], chart: Set[Edge])

object Parser {
  def parse(grammar: Grammar, sentence: Seq[Terminal[_]]): Set[Edge] = {
    val initialAgenda = initBottomUpAgenda(grammar, sentence)
    val initialChart = Set[Edge]()
    val chart = step(grammar, chartRules, initialAgenda, initialChart)
    // todo: group to ASTs
    chart
  }

  def initBottomUpAgenda(grammar: Grammar, sentence: Seq[Terminal[_]]): List[Edge] = {
    sentence.zipWithIndex.map {
      case (terminal, index) => TerminalEdge(index, terminal)
    }.toList
  }

  // generates new edges to be added to agenda
  // todo - probably pass this function into the loop to configure algorithm
  def chartRules(edge: Edge, chart: Set[Edge]): List[Edge] = {
    // fundamental rule, hypothesis rule...
    ???
  }

  @tailrec
  private final def step(grammar: Grammar,
                         chartRules: (Edge, Set[Edge]) => List[Edge],
                         agenda: List[Edge],
                         chart: Set[Edge]): Set[Edge] = agenda match {
    case Nil => chart
    case edge::edges =>
      val newEdges = chartRules(edge, chart)
      val newAgenda = edges ++ newEdges // TODO: dfs vs bfs ?
      val newChart = chart + edge
      step(grammar, chartRules, newAgenda, newChart)
  }
}
