package chapa

object TestGrammar extends Grammar {
  val rootSymbol = S

  val rules = Set[Rule](
    Rule(S, AGG, GROUP, (agg: AggExpr, group: GroupByExpr) => QueryExpr(agg, Some(group), None)),
    Rule(S, AGG, FILTER, (agg: AggExpr, filter: FilterExpr) => QueryExpr(agg, None, Some(filter))),
    Rule(S, AGG, GROUP, FILTER, (agg: AggExpr, group: GroupByExpr, filter: FilterExpr) => QueryExpr(agg, Some(group), Some(filter))),

    Rule(AGG, Aggregate, (agg: AggregateExpr) => AggExpr(agg)),

    Rule(GROUP, WORD("by"), Dim, (_: StringExpr, dim: DimExpr) => GroupByExpr(dim)),

    Rule(FILTER, WORD("where"), B_EXPR, (_: StringExpr, bExpr: BExpr) => FilterExpr(bExpr)),
    Rule(FILTER, B_EXPR, (bExpr: BExpr) => FilterExpr(bExpr)),

    Rule(B_EXPR, COND, (cond: CondExpr) => cond),
    Rule(B_EXPR, AND, (and: AndExpr) => and),

    Rule(AND, B_EXPR, WORD("and"), B_EXPR, (expr1: BExpr, _: StringExpr, expr2: BExpr) => AndExpr(expr1, expr2)),

    Rule(COND, Dim, Entity, (dim: DimExpr, ent: EntityExpr) => CondExpr(dim, ent)),
  )

  // todo: add some type safety
  override def tokenToTerminal(token: String): Terminal[Expr] = token match {
    case r"agg.*" => Aggregate
    case r"dim.*" => Dim
    case r"ent.*" => Entity
    case s => WORD(s)
  }

  implicit class Regex(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }
}
