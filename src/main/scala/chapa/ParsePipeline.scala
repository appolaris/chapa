package chapa

object ParsePipeline {
  def parse(grammar: Grammar, str: String, trace: Boolean = false): List[Edge] = {
    val sentence = str.split(' ').map(grammar.tokenToTerminal)
    val terminals = sentence.zipWithIndex.map {
      case (terminal, index) => TerminalEdge(index, terminal)
    }.toList

    val res1 = ChartParser.parse(RulesPhase1.chartRules, grammar, terminals)
    if (trace) printTrace("Initial", res1)

    val inactive = res1.filter(_.isInactive)
    if (trace) printTrace("Inactive", inactive)

    val roots = inactive -- inactive.flatMap(_.children.flatten)
    if (trace) printTrace("Roots", roots)

    val combined = ChartParser.parse(RulesPhase2.chartRules, grammar, roots.toList)
    if (trace) printTrace("Combined", combined)

    val purged = Purger.purge(grammar.rootSymbol, combined)
    if (trace) printTrace("Purged", purged)

    purged
  }

  private def printTrace(header: String, edges: Iterable[Edge]): Unit = {
    println(s"-- $header --")
    println(edges.map(prettyPrint).mkString("\n"))
    println()
  }
}
