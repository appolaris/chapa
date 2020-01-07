package chapa

case class AggExpr(s: String) extends Expr
case class DimExpr(s: String) extends Expr
case class GroupByExpr(dim: Option[DimExpr]) extends Expr
case class FilterExpr(bExpr: BExpr) extends Expr
case class EntityExpr(s: String) extends Expr
case class StringExpr(i: String) extends Expr

sealed trait BExpr extends Expr
case class AndExpr(expr1: BExpr, expr2: BExpr) extends BExpr
case class CondExpr(dim: DimExpr, ent: EntityExpr) extends BExpr


