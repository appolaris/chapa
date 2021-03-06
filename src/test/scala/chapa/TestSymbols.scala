package chapa

case object Aggregate extends Terminal[AggregateExpr]
case object Dim extends Terminal[DimExpr]
case object Entity extends Terminal[EntityExpr]
case class WORD(s: String) extends Terminal[StringExpr]

case object S extends NonTerminal[QueryExpr]
case object AGG extends NonTerminal[AggExpr]
case object GROUP extends NonTerminal[GroupByExpr]
case object FILTER extends NonTerminal[FilterExpr]
case object ENTRY extends NonTerminal[EntityExpr]
case object B_EXPR extends NonTerminal[BExpr]
case object AND extends NonTerminal[AndExpr]
case object COND extends NonTerminal[CondExpr]

