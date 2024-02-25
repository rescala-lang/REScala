package lore.dsl

import scala.quoted.{Expr, Quotes, Type}

def constructUnboundInteractionWithRequires[S, A](requires: Expr[Seq[Requires[S, A]]],
                                                  ensures: Expr[Seq[Ensures[S, A]]],
                                                  expr: Expr[(S, A) => Boolean],
                                                  invariantManager: Expr[InvariantManager])
                                                 (using Quotes, Type[S], Type[A]): Expr[UnboundInteraction[S, A]] =
  '{ UnboundInteraction[S, A]($requires :+ Requires($expr, ${ showPredicateCode(expr) }), $ensures, $invariantManager) }

def constructUnboundInteractionWithEnsures[S, A](requires: Expr[Seq[Requires[S, A]]],
                                                 ensures: Expr[Seq[Ensures[S, A]]],
                                                 expr: Expr[(S, A) => Boolean],
                                                 invariantManager: Expr[InvariantManager])
                                                (using Quotes, Type[S], Type[A]): Expr[UnboundInteraction[S, A]] =
  '{ UnboundInteraction[S, A]($requires, $ensures :+ Ensures($expr, ${ showPredicateCode(expr) }), $invariantManager) }

case class UnboundInteraction[S, A] private[dsl](private[dsl] val requires: Seq[Requires[S, A]] = Seq.empty,
                                                 private[dsl] val ensures: Seq[Ensures[S, A]] = Seq.empty,
                                                 private[dsl] val invariantManager: InvariantManager)
  extends Interaction[S, A] with CanModify[S, A] with CanExecute[S, A] {
  type T[_, _] = UnboundInteraction[S, A]
  type E[_, _] = InteractionWithExecutes[S, A]
  type M[_, _] = InteractionWithModifies[S, A]

  override inline def requires(inline pred: (S, A) => Boolean): UnboundInteraction[S, A] =
    ${ constructUnboundInteractionWithRequires('requires, 'ensures, 'pred, 'invariantManager) }

  override inline def ensures(inline pred: (S, A) => Boolean): UnboundInteraction[S, A] =
    ${ constructUnboundInteractionWithEnsures('requires, 'ensures, 'pred, 'invariantManager) }

  override inline def modifies(inline expr: Source[S]): InteractionWithModifies[S, A] =
    InteractionWithModifies(requires, ensures, expr, invariantManager)

  override inline def executes(inline fun: (S, A) => S): InteractionWithExecutes[S, A] =
    InteractionWithExecutes(requires, ensures, fun, invariantManager)
}
