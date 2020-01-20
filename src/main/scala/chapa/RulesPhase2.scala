package chapa

object RulesPhase2 {
  // generates new edges to be added to agenda
  def chartRules(grammar: Grammar, edge: Edge, chart: Set[Edge]): List[Edge] = {
    // Fundamental rule
    val fundamental = chart.flatMap(chartEdge => fundamentalRule(edge, chartEdge))

    // Prediction rule
    // TODO - probably check if such edge already exists
    // Ignore rules which were already used to handle recursive rules
    val usedRules = getSubEdges(edge).collect { case e: RuleEdge => e.rule }
    val predicted = grammar.rules.diff(usedRules).flatMap(r => r.rhs.zipWithIndex.collect {
      case (symbol, idx) if symbol == edge.symbol =>
        RuleEdge(edge.start, edge.end, r, Vector.fill(r.rhs.length)(None).updated(idx, Some(edge)))
    })

    fundamental.toList ++ predicted
  }

  def fundamentalRule(agendaEdge: Edge, chartEdge: Edge): Seq[Edge] = (agendaEdge, chartEdge) match {
    case (e1: Edge, e2: RuleEdge) if e2.isActive => fundamentalRuleImpl(e1, e2)
    case (e1: RuleEdge, e2: Edge) if e1.isActive => fundamentalRuleImpl(e2, e1)
    case _ => Seq()
  }

  def fundamentalRuleImpl(edge: Edge, ruleEdge: RuleEdge): Seq[Edge] = {
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
}
