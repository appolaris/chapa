package chapa

sealed trait Symbol[+E]
trait Terminal[+E] extends Symbol[E]
trait NonTerminal[E] extends Symbol[E]

