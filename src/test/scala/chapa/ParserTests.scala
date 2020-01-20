package chapa

import org.scalatest.FunSuite

import scala.io.AnsiColor.{RED, RESET}

class ParserTests extends FunSuite {
  //todo: add actual tests
  test("test parsing") {

    val sentence = "agg by".split(' ').map(tokenToTerminal)
    val terminals = sentence.zipWithIndex.map {
      case (terminal, index) => TerminalEdge(index, terminal)
    }.toList

    val res1 = ChartParser.parse(RulesPhase1.chartRules, TestGrammar, terminals)
    println("-- Initial --")
    println(res1.map(prettyPrint).mkString("\n"))
    println()

    val inactive = res1.filter(_.isInactive)
    println("-- Inactive --")
    println(inactive.map(prettyPrint).mkString("\n"))
    println()

    val roots = inactive -- inactive.flatMap(_.children.flatten)
    println("-- Roots --")
    println(roots.map(prettyPrint).mkString("\n"))
    println()

    val combined = ChartParser.parse(RulesPhase2.chartRules, TestGrammar, roots.toList)
    println("-- Combined --")
    println(combined.map(prettyPrint).mkString("\n"))
    println()

    val purged = Purger.purge(S, combined)
    println("-- Purged --")
    println(purged.map(prettyPrint).mkString("\n"))
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

  private def prettyPrint(edge: Edge): String = edge match {
    case e: TerminalEdge => s"${e.start}, ${e.end}, ${edge.symbol}"
    case e: RuleEdge => s"""${e.start}, ${e.end}, ${e.rule.lhs} => ${e.rule.rhs.zip(e.children).map(x => if (x._2.isDefined) x._1 else s"$RED${x._1}$RESET").mkString(" ")}"""
  }
}

