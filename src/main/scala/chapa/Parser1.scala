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

  def fundamentalRule(agendaEdge: Edge, chartEdge: Edge): Seq[Edge] = (agendaEdge, chartEdge) match {
    case (e1: Edge, e2: RuleEdge) if e1.isInactive && e2.isActive => fundamentalRule2(e1, e2)
    case (e1: RuleEdge, e2: Edge) if e1.isActive && e2.isInactive => fundamentalRule2(e2, e1)
    case _ => Seq()
  }

  def fundamentalRule2(inactiveEdge: Edge, activeEdge: RuleEdge): Seq[Edge] = {
    if (activeEdge.end != inactiveEdge.start) return Vector()

    val firstUnprocessedSymbol = activeEdge.rule.rhs
      .zipWithIndex.zip(activeEdge.children)
      .collectFirst { case (symbolAndIndex, None) => symbolAndIndex }

    firstUnprocessedSymbol.collectFirst {
      case (symbol, idx) if symbol == inactiveEdge.symbol =>
        RuleEdge(activeEdge.start, inactiveEdge.end, activeEdge.rule, activeEdge.children.updated(idx, Some(inactiveEdge)))
    }.toSeq
  }

  // generates new edges to be added to agenda
  // todo - probably pass this function into the loop to configure algorithm
  def chartRules(grammar: Grammar, edge: Edge, chart: Set[Edge]): List[Edge] = {
    // Fundamental rule
    val fundamental = chart.flatMap(chartEdge => fundamentalRule(edge, chartEdge))

    // Prediction rule for INACTIVE edge only - add active edges for all rules which have RHS beginning with symbol of this INACTIVE edge
    val predicted = if (edge.isInactive) {
      grammar.rules.filter(r => r.rhs.head == edge.symbol).map(r =>
        RuleEdge(edge.start, edge.end, r, Some(edge) +: Vector.fill(r.rhs.length - 1)(None))
      )
    }
    else
      Nil

    fundamental.toList ++ predicted
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
