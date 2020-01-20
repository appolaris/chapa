package chapa

trait Grammar {
  def rootSymbol: Symbol[_]
  def rules: Set[Rule]
  def tokenToTerminal(token: String): Terminal[Expr]
}
