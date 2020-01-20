package chapa

import org.scalatest.FunSuite


class PurgerTests extends FunSuite {
  val RULE_S = Rule(S, AGG, GROUP, null)
  val RULE_AGG = Rule(AGG, Aggregate, null)

  test("Purge lest complete edges") {
    val child = Some(TerminalEdge(0,  WORD("x")))
    val edge1 = RuleEdge(0, 1, RULE_S, Vector(child, child))
    val edge2 = RuleEdge(1, 2, RULE_S, Vector(child, None))
    val edge3 = RuleEdge(1, 2, RULE_S, Vector(None, child))
    assert(Purger.purge(Set(edge1, edge2, edge3)) == List(edge1))
  }

  test("RuleEdge == RuleEdge") {
    val child = Some(TerminalEdge(0,  WORD("x")))
    val edge1 = RuleEdge(0, 1, RULE_S, Vector(child, None))
    val edge2 = RuleEdge(1, 2, RULE_S, Vector(child, None))
    assert(Purger.compareEdges(edge1, edge2).contains(0))
  }

  test("TerminalEdge == TerminalEdge") {
    val edge1 = TerminalEdge(0,  WORD("x"))
    val edge2 = TerminalEdge(1,  WORD("x")) // index ignored
    assert(Purger.compareEdges(edge1, edge2).contains(0))
  }

  test("TerminalEdge <INCOMPARABLE> TerminalEdge") {
    val edge1 = TerminalEdge(0,  WORD("x"))
    val edge2 = TerminalEdge(0,  WORD("y"))
    assert(Purger.compareEdges(edge1, edge2).isEmpty)
  }

  test("RuleEdge <INCOMPARABLE> RuleEdge") {
    val RULE_S = Rule(S, AGG, GROUP, null)
    val child = Some(TerminalEdge(0,  WORD("x")))
    val edge1 = RuleEdge(0, 1, RULE_S, Vector(child, None))
    val edge2 = RuleEdge(1, 2, RULE_S, Vector(None, child))
    assert(Purger.compareEdges(edge1, edge2).isEmpty)
  }

  test("TerminalEdge <INCOMPARABLE> RuleEdge") {
    val RULE_S = Rule(S, AGG, GROUP, null)
    val edge1 = TerminalEdge(0,  WORD("x"))
    val edge2 = RuleEdge(1, 2, RULE_S, Vector(None, None))
    assert(Purger.compareEdges(edge1, edge2).isEmpty)
  }

  test("RuleEdge > RuleEdge") {
    val child = Some(TerminalEdge(0,  WORD("x")))
    val edge1 = RuleEdge(0, 1, RULE_S, Vector(child, None))
    val edge2 = RuleEdge(1, 2, RULE_S, Vector(None, None))
    assert(Purger.compareEdges(edge1, edge2).contains(+1))
  }

  test("RuleEdge < RuleEdge (deep)") {
    val child11 = Some(TerminalEdge(0,  Aggregate))
    val child1 = RuleEdge(0, 1, RULE_AGG, Vector(None))
    val child2 = RuleEdge(0, 1, RULE_AGG, Vector(child11))
    val edge1 = RuleEdge(0, 1, RULE_S, Vector(None, Some(child1)))
    val edge2 = RuleEdge(1, 2, RULE_S, Vector(None, Some(child2)))
    assert(Purger.compareEdges(edge1, edge2).contains(-1))
  }
}
