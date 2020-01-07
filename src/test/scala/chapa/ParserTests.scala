package chapa

import org.scalatest.FunSuite

class ParserTests extends FunSuite {
  test("test parsing") {
    Parser.parse(TestGrammar, "agg by dim".split(' ').map(tokenToTerminal))
  }

  // todo: add some type safety
  def tokenToTerminal(token: String): Terminal[Expr] = token match {
    case r"(agg.*)" => Agg
    case r"(dim.*)" => Dim
    case r"(ent.*)" => Entity
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

