import scala.io.AnsiColor.{BLUE, RED, RESET}

package object chapa {
  def prettyPrint(edge: Edge): String = edge match {
    case e: TerminalEdge => s"${e.start}, ${e.end}, ${e.symbol}"
    case e: RuleEdge => s"""${e.start}, ${e.end}, ${e.rule.lhs} => ${e.rule.rhs.zip(e.children).map(x => if (x._2.isDefined) x._1 else s"$RED${x._1}$RESET").mkString(" ")}"""
  }

  def deepPrint(edge: Edge): String = edge match {
    case e: TerminalEdge => s"$BLUE${e.symbol.toString}$RESET"
    case e: RuleEdge => s"""(${e.rule.lhs} => ${(e.children zip e.rule.rhs).map(x => x._1.map(deepPrint).getOrElse(s"$RED${x._2}$RESET")).mkString(" ")})"""
  }
}
