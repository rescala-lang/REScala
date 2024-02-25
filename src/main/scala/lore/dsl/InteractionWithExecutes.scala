package lore.dsl

import scala.quoted.{Expr, Quotes, Type}

def constructInteractionWithExecutesWithRequires[S, A](requires: Expr[Seq[Requires[S, A]]],
                                                       ensures: Expr[Seq[Ensures[S, A]]],
                                                       executes: Expr[(S, A) => S],
                                                       expr: Expr[(S, A) => Boolean],
                                                       invariantManager: Expr[InvariantManager])
                                                      (using Quotes, Type[S], Type[A]): Expr[InteractionWithExecutes[S, A]] =
  '{ InteractionWithExecutes[S, A]($requires :+ Requires($expr, ${ showPredicateCode(expr) }), $ensures, $executes, $invariantManager) }

def constructInteractionWithExecutesWithEnsures[S, A](requires: Expr[Seq[Requires[S, A]]],
                                                      ensures: Expr[Seq[Ensures[S, A]]],
                                                      executes: Expr[(S, A) => S],
                                                      expr: Expr[(S, A) => Boolean],
                                                      invariantManager: Expr[InvariantManager])
                                                     (using Quotes, Type[S], Type[A]): Expr[InteractionWithExecutes[S, A]] =
  '{ InteractionWithExecutes[S, A]($requires, $ensures :+ Ensures($expr, ${ showPredicateCode(expr) }), $executes, $invariantManager) }

case class InteractionWithExecutes[S, A] private[dsl](private[dsl] val requires: Seq[Requires[S, A]] = Seq.empty,
                                                      private[dsl] val ensures: Seq[Ensures[S, A]] = Seq.empty,
                                                      private[dsl] val executes: (S, A) => S,
                                                      private[dsl] val invariantManager: InvariantManager)
  extends Interaction[S, A]
    with CanModify[S, A] {
  type T[_, _] = InteractionWithExecutes[S, A]
  type M[_, _] = BoundInteraction[S, A]

  override inline def requires(inline pred: (S, A) => Boolean): InteractionWithExecutes[S, A] =
    ${ constructInteractionWithExecutesWithRequires('requires, 'ensures, 'executes, 'pred, 'invariantManager) }

  override inline def ensures(inline pred: (S, A) => Boolean): InteractionWithExecutes[S, A] =
    ${ constructInteractionWithExecutesWithEnsures('requires, 'ensures, 'executes, 'pred, 'invariantManager) }

  override inline def modifies(inline expr: Source[S]): BoundInteraction[S, A] =
    BoundInteraction(requires, ensures, executes, expr, invariantManager)
}
