package lore.dsl

import reactives.core.ReSource
import reactives.default.*
import reactives.operator.Interface.State as BundleState


import scala.quoted.{Expr, Quotes, Type}

def constructInteractionWithModifiesWithRequires[ST <: Tuple, S <: Tuple, A](interaction: Expr[InteractionWithModifies[ST, S, A]],
                                                           expr: Expr[(ST, A) => Boolean])
                                                          (using Quotes, Type[ST], Type[S], Type[A]): Expr[InteractionWithModifies[ST, S, A]] = '{
  val (inputs, fun, isStatic) =
    reactives.macros.MacroLegos.getDependencies[(ST, A) => Boolean, ReSource.of[BundleState], reactives.core.StaticTicket[BundleState], true]($expr)

  $interaction.copy(requires = $interaction.requires :+ Requires(inputs, fun, ${ showPredicateCode(expr) }))
}

def constructInteractionWithModifiesWithEnsures[ST <: Tuple, S <: Tuple, A](interaction: Expr[InteractionWithModifies[ST, S, A]],
                                                          expr: Expr[(ST, A) => Boolean])
                                                         (using Quotes, Type[ST], Type[S], Type[A]): Expr[InteractionWithModifies[ST, S, A]] = '{
  val (inputs, fun, isStatic) =
    reactives.macros.MacroLegos.getDependencies[(ST, A) => Boolean, ReSource.of[BundleState], reactives.core.StaticTicket[BundleState], true]($expr)

  val newEns = Ensures(inputs, fun, ${ showPredicateCode(expr) })

  $interaction.copy(ensures = $interaction.ensures :+ newEns)
}

case class InteractionWithModifies[ST <: Tuple, S <: Tuple, A] private[dsl](private[dsl] val requires: Seq[Requires[ST, A]] = Seq.empty,
                                                          private[dsl] val ensures: Seq[Ensures[ST, A]] = Seq.empty,
                                                          private[dsl] val modifies: S)
  extends Interaction[ST, A]
    with CanExecute[ST, A] {
  type T[_, _] = InteractionWithModifies[ST, S, A]
  type E[_, _] = BoundInteraction[ST, S, A]

  override inline def requires(inline pred: (ST, A) => Boolean): InteractionWithModifies[ST, S, A] =
    ${ constructInteractionWithModifiesWithRequires('{ this }, '{ pred }) }

  override inline def ensures(inline pred: (ST, A) => Boolean): InteractionWithModifies[ST, S, A] =
    ${ constructInteractionWithModifiesWithEnsures('{ this }, '{ pred }) }

  override def executes(fun: (ST, A) => ST): BoundInteraction[ST, S, A] =
    BoundInteraction(requires, ensures, fun, modifies)
}
