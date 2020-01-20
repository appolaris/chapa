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

    val parents = inactive -- inactive.flatMap(_.children.flatten)
    if (trace) printTrace("Parents", parents)

    val combined = ChartParser.parse(RulesPhase2.chartRules, grammar, parents.toList)
    if (trace) printTrace("Combined", combined)

    val roots = combined.filter(_.symbol == grammar.rootSymbol)
    if (trace) printTrace("Roots", roots)

    val withRequired = roots.filter(r => containsSubEdges(r, inactive))
    if (trace) printTrace("Roots with required edges", withRequired)

    val purged = Purger.purge(withRequired)
    if (trace) printTrace("Purged", purged)

    purged
  }

  private def containsSubEdges(parentEdge: Edge, requiredEdges: Iterable[Edge]) = {
    val subEdges = getSubEdges(parentEdge)
    requiredEdges.forall(e => subEdges.contains(e))
  }

  private def getSubEdges(edge: Edge): Set[Edge] = {
    (edge.children.flatten ++ edge.children.flatten.flatMap(getSubEdges)).toSet
  }

  private def printTrace(header: String, edges: Iterable[Edge]): Unit = {
    println(s"-- $header --")
    println(edges.map(prettyPrint).mkString("\n"))
    println()
  }
}
