package chapa

import org.scalatest.FunSuite

class ParserTests extends FunSuite {
  //todo: add actual tests
  test("test parsing") {

    val sentence = "agg by".split(' ').map(tokenToTerminal)
    val terminals = sentence.zipWithIndex.map {
      case (terminal, index) => TerminalEdge(index, terminal)
    }.toList

    val res1 = ChartParser.parse(RulesPhase1.chartRules, TestGrammar, terminals)
    println("-- Initial --")
    println(res1.mkString("\n"))
    println()

    val inactive = res1.filter(_.isInactive)
    println("-- Inactive --")
    println(inactive.mkString("\n"))
    println()

    val roots = inactive -- inactive.flatMap(_.children.flatten)
    println("-- Roots --")
    println(roots.mkString("\n"))
    println()

    val combined = ChartParser.parse(RulesPhase2.chartRules, TestGrammar, roots.toList)
    println("-- Combined --")
    println(combined.mkString("\n"))
    println()

    val fromS = combined.filter(_.symbol == S)
    println("-- S --")
    println(fromS.mkString("\n"))
    println()
  }

  // todo: add some type safety
  def tokenToTerminal(token: String): Terminal[Expr] = token match {
    case r"agg*" => Aggregate
    case r"dim*" => Dim
    case r"ent*" => Entity
    case s => WORD(s)
  }

//  // todo: add some type safety
//  override def tokenToExpression(token: String): Expr = token match {
//    case r"(agg.*)$s" => AggExpr(s)
//    case r"(dim.*)$s" => DimExpr(s)
//    case r"(ent.*)$s" => EntityExpr(s)
//    case s => StringExpr(s)
//  }

  implicit class Regex(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }
}

