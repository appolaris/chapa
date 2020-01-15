package chapa

sealed trait Edge {
  def start: Int
  def end: Int
//  def isActive: Boolean
  def symbol: Symbol[_]
  def children: Vector[Edge]
  def unprocessed: Vector[Symbol[_]]
}

case class TerminalEdge(start: Int, terminal: Terminal[_]) extends Edge {
  override def end: Int = start + 1
//  override val isActive: Boolean = false
  override def symbol: Symbol[_] = terminal
  override def children: Vector[Edge] = Vector()
  override def unprocessed: Vector[Symbol[_]] = Vector()
  override def toString: String = s"$start, $end, $symbol •"
}

case class RuleEdge(start: Int, end: Int, rule: Rule, children: Vector[Edge], unprocessed: Vector[Symbol[_]]) extends Edge {
//  override lazy val isActive: Boolean = children.length < rule.rhs.length
  override def symbol: Symbol[_] = rule.lhs

  //  lazy val nextSymbol: Symbol[_] = rule.rhs(children.length)
  //  val unprocessded: List[Symbol[_]] = ???
  override def toString: String = s"$start, $end, ${rule.lhs} => ${children.map(_.symbol).mkString(" ")} • ${unprocessed.mkString(" ")}"
}
