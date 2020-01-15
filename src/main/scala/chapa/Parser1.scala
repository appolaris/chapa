package chapa

import scala.annotation.tailrec

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
  def chartRules(grammar: Grammar, edge: Edge, chart: Set[Edge]): List[Edge] = {
    // Fundamental rule
    val filled = edge match {
      case inactiveEdge if inactiveEdge.unprocessed.isEmpty =>
        // Edge is inactive - find all ACTIVE edges which can be (partially?) filled by this INACTIVE edge
        chart.collect {
          case RuleEdge(start, end, rule, processed, nextSymbol +: unprocessed) if end == edge.start && nextSymbol == edge.symbol =>
            RuleEdge(start, edge.end, rule, processed :+ edge, unprocessed)
        }
      case activeEdge: RuleEdge if activeEdge.unprocessed.nonEmpty =>
        // Edge is ACTIVE - find all INACTIVE edges which can complete this ACTIVE edge
        chart.collect {
          case inactiveEdge if inactiveEdge.unprocessed.isEmpty && activeEdge.end == inactiveEdge.start && activeEdge.unprocessed.head == inactiveEdge.symbol =>
            RuleEdge(activeEdge.start, inactiveEdge.end, activeEdge.rule , activeEdge.children :+ inactiveEdge, activeEdge.unprocessed.tail)
        }
    }

    // Prediction rule for INACTIVE edge only - add active edges for all rules which have RHS beginning with symbol of this INACTIVE edge
    val predicted = if (edge.unprocessed.isEmpty) {
      grammar.rules.filter(r => r.rhs.head == edge.symbol).map(r =>
        RuleEdge(edge.start, edge.end, r, Vector(edge), r.rhs.tail)
      )
    }
    else
      Nil

    filled.toList ++ predicted
  }

  @tailrec
  private final def step(grammar: Grammar,
                         chartRules: (Grammar, Edge, Set[Edge]) => List[Edge],
                         agenda: List[Edge],
                         chart: Set[Edge]): Set[Edge] = agenda match {
    case Nil => chart
    case edge::edges =>
      val newEdges = chartRules(grammar, edge, chart)
      val newAgenda = edges ++ newEdges // TODO: dfs vs bfs ?
      val newChart = chart + edge
      step(grammar, chartRules, newAgenda, newChart)
  }
}
