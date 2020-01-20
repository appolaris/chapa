package chapa

import scala.annotation.tailrec

object Parser2 {
  def parse(grammar: Grammar, agenda: Set[Edge]): Set[Edge] = {
    val initialChart = Set[Edge]()
    val chart = step(grammar, chartRules, agenda.toList, initialChart)
    chart
  }

  def fundamentalRule(agendaEdge: Edge, chartEdge: Edge): Seq[Edge] = (agendaEdge, chartEdge) match {
    case (e1: Edge, e2: RuleEdge) if e2.isActive => fundamentalRule2(e1, e2)
    case (e1: RuleEdge, e2: Edge) if e1.isActive => fundamentalRule2(e2, e1)
    case _ => Seq()
  }

  def fundamentalRule2(edge: Edge, ruleEdge: RuleEdge): Seq[Edge] = {
    val unprocessedSymbols = ruleEdge.rule.rhs
      .zipWithIndex.zip(ruleEdge.children)
      .collect { case (symbolAndIndex, None) => symbolAndIndex }

    unprocessedSymbols.collect {
      case (symbol, idx) if symbol == edge.symbol =>
        // Since we can combine edges in any order and allow "holes" between child edges - start & end are rather imprecise
        val start = Math.min(edge.start, ruleEdge.start)
        val end = Math.max(edge.end, ruleEdge.end)
        RuleEdge(start, end, ruleEdge.rule, ruleEdge.children.updated(idx, Some(edge)))
    }
  }

  // generates new edges to be added to agenda
  def chartRules(grammar: Grammar, edge: Edge, chart: Set[Edge]): List[Edge] = {
    // Fundamental rule
    val fundamental = chart.flatMap(chartEdge => fundamentalRule(edge, chartEdge))

    // Prediction rule
    // TODO - probably check if such edge already exists
    val predicted = grammar.rules.flatMap(r => r.rhs.zipWithIndex.collect {
      case (symbol, idx) if symbol == edge.symbol =>
        RuleEdge(edge.start, edge.end, r, Vector.fill(r.rhs.length)(None).updated(idx, Some(edge)))
    })

    fundamental.toList ++ predicted
  }

  //  def firstUnprocessed(edge: RuleEdge): Option[Symbol[_]] = rule.rhs.zip(children).collectFirst { case (symbol, None) => symbol }

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
