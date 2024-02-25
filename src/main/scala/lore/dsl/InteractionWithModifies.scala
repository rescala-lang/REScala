package lore.dsl

import scala.quoted.{Expr, Quotes, Type}

def constructInteractionWithModifiesWithRequires[S, A](requires: Expr[Seq[Requires[S, A]]],
                                                       ensures: Expr[Seq[Ensures[S, A]]],
                                                       modifies: Expr[Source[S]],
                                                       expr: Expr[(S, A) => Boolean],
                                                       invariantManager: Expr[InvariantManager])
                                                      (using Quotes, Type[S], Type[A]): Expr[InteractionWithModifies[S, A]] =
  '{ InteractionWithModifies[S, A]($requires :+ Requires($expr, ${ showPredicateCode(expr) }), $ensures, $modifies, $invariantManager) }

def constructInteractionWithModifiesWithEnsures[S, A](requires: Expr[Seq[Requires[S, A]]],
                                                      ensures: Expr[Seq[Ensures[S, A]]],
                                                      modifies: Expr[Source[S]],
                                                      expr: Expr[(S, A) => Boolean],
                                                      invariantManager: Expr[InvariantManager])
                                                     (using Quotes, Type[S], Type[A]): Expr[InteractionWithModifies[S, A]] =
  '{ InteractionWithModifies[S, A]($requires, $ensures :+ Ensures($expr, ${ showPredicateCode(expr) }), $modifies, $invariantManager) }

case class InteractionWithModifies[S, A] private[dsl](private[dsl] val requires: Seq[Requires[S, A]] = Seq.empty,
                                                      private[dsl] val ensures: Seq[Ensures[S, A]] = Seq.empty,
                                                      private[dsl] val modifies: Source[S],
                                                      private[dsl] val invariantManager: InvariantManager)
  extends Interaction[S, A]
    with CanExecute[S, A] {
  type T[_, _] = InteractionWithModifies[S, A]
  type E[_, _] = BoundInteraction[S, A]

  override inline def requires(inline pred: (S, A) => Boolean): InteractionWithModifies[S, A] =
    ${ constructInteractionWithModifiesWithRequires('requires, 'ensures, 'modifies, 'pred, 'invariantManager) }

  override inline def ensures(inline pred: (S, A) => Boolean): InteractionWithModifies[S, A] =
    ${ constructInteractionWithModifiesWithEnsures('requires, 'ensures, 'modifies, 'pred, 'invariantManager) }

  override inline def executes(inline fun: (S, A) => S): BoundInteraction[S, A] =
    BoundInteraction(requires, ensures, fun, modifies, invariantManager)
}
