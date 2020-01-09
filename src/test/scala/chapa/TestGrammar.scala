package chapa

object TestGrammar extends Grammar {
  val rules = Set[Rule](
    Rule(S, AGG, GROUP, (agg: AggExpr, group: GroupByExpr) => QueryExpr(agg, group)),

    Rule(AGG, Aggregate, (agg: AggregateExpr) => AggExpr(agg)),

    Rule(GROUP, WORD("by"), Dim, (_: StringExpr, dim: DimExpr) => GroupByExpr(dim)),

    Rule(FILTER, WORD("where"), B_EXPR, (_: StringExpr, bExpr: BExpr) => FilterExpr(bExpr)),
    Rule(FILTER, B_EXPR, (bExpr: BExpr) => FilterExpr(bExpr)),

    Rule(B_EXPR, COND, (cond: CondExpr) => cond),
    Rule(B_EXPR, AND, (and: AndExpr) => and),

    Rule(AND, B_EXPR, WORD("and"), B_EXPR, (expr1: BExpr, _: StringExpr, expr2: BExpr) => AndExpr(expr1, expr2)),

    Rule(COND, Dim, Entity, (dim: DimExpr, ent: EntityExpr) => CondExpr(dim, ent)),
  )



}
