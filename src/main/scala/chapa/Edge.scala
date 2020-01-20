package chapa

import io.AnsiColor._

sealed trait Edge {
  def start: Int
  def end: Int
  def symbol: Symbol[_]
  def children: Vector[Option[Edge]]
  lazy val isActive: Boolean = children.exists(_.isEmpty)
  lazy val isInactive: Boolean = !isActive
}

case class TerminalEdge(start: Int, terminal: Terminal[_]) extends Edge {
  override def end: Int = start + 1
  override def symbol: Symbol[_] = terminal
  override def children: Vector[Option[Edge]] = Vector()
  override def toString: String = s"$start, $end, $symbol"
}

case class RuleEdge(start: Int, end: Int, rule: Rule, children: Vector[Option[Edge]]) extends Edge {
  override def symbol: Symbol[_] = rule.lhs
  override def toString: String = s"""$start, $end, ${rule.lhs} => ${rule.rhs.zip(children).map(x => if (x._2.isDefined) x._1 else s"$RED${x._1}$RESET").mkString(" ")}"""
}
