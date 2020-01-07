package chapa

case class Rule(lhs: Symbol[_], rhs: List[Symbol[_]], semFn: List[Expr] => Expr)

object Rule {
  def apply[T <: Expr, T1 <: Expr, T2 <: Expr, T3 <: Expr](
        lhs: Symbol[T],
        rhs1: Symbol[T1],
        rhs2: Symbol[T2],
        rhs3: Symbol[T3],
        semFn: (T1, T2, T3) => T
  ): Rule = {
    new Rule(
        lhs,
        List(rhs1, rhs2, rhs3),
        xs => semFn(xs(0).asInstanceOf[T1], xs(1).asInstanceOf[T2], xs(2).asInstanceOf[T3]))
  }

  def apply[T <: Expr, T1 <: Expr, T2 <: Expr](
        lhs: Symbol[T],
        rhs1: Symbol[T1],
        rhs2: Symbol[T2],
        semFn: (T1, T2) => T
  ): Rule = new Rule(lhs, List(rhs1, rhs2), xs => semFn(xs(0).asInstanceOf[T1], xs(1).asInstanceOf[T2]))

  def apply[T <: Expr, T1 <: Expr](lhs: Symbol[T], rhs1: Symbol[T1], semFn: T1 => T): Rule = {
    new Rule(lhs, List(rhs1), xs => semFn.apply(xs.head.asInstanceOf[T1]))
  }
}
