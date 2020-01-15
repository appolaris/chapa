package chapa

import org.scalatest.FunSuite

class ParserTests extends FunSuite {
  //todo: add actual tests
  test("test parsing") {
    val res1 = Parser.parse(TestGrammar, "agg by".split(' ').map(tokenToTerminal))
    println("-- Initial --")
    println(res1.mkString("\n"))
    println()

    val inactive = res1.filter(_.unprocessed.isEmpty)
    println("-- Inactive --")
    println(inactive.mkString("\n"))
    println()

    val roots = inactive -- inactive.flatMap(_.children)
    println("-- Roots --")
    println(roots.mkString("\n"))
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

