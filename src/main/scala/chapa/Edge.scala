package chapa

sealed trait Edge {
  def start: Int
  def end: Int
}

case class TerminalEdge(start: Int, terminal: Terminal[_]) extends Edge {
  def end: Int = start + 1
}

case class RuleEdge(start: Int, end: Int, rule: Rule, children: List[Edge]) extends Edge {
  lazy val isActive: Boolean = children.length >= rule.rhs.length
}
