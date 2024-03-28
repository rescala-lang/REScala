package lore.dsl

import rescala.core.ReSource
import rescala.default.BundleState

import scala.quoted.{Expr, Quotes, Type}

def constructInteractionWithExecutesWithRequires[S <: Tuple, A](interaction: Expr[InteractionWithExecutes[S, A]],
                                                       expr: Expr[(S, A) => Boolean])
                                                      (using Quotes, Type[S], Type[A]): Expr[InteractionWithExecutes[S, A]] = '{
  val (inputs, fun, isStatic) =
    rescala.macros.getDependencies[(S, A) => Boolean, ReSource.of[BundleState], rescala.core.StaticTicket[BundleState], true]($expr)

  $interaction.copy(requires = $interaction.requires :+ Requires(inputs, fun, ${ showPredicateCode(expr) }))
}

def constructInteractionWithExecutesWithEnsures[S <: Tuple, A](interaction: Expr[InteractionWithExecutes[S, A]],
                                                      expr: Expr[(S, A) => Boolean])
                                                     (using Quotes, Type[S], Type[A]): Expr[InteractionWithExecutes[S, A]] = '{
  val (inputs, fun, isStatic) =
    rescala.macros.getDependencies[(S, A) => Boolean, ReSource.of[BundleState], rescala.core.StaticTicket[BundleState], true]($expr)

  $interaction.copy(ensures = $interaction.ensures :+ Ensures(inputs, fun, ${ showPredicateCode(expr) }))
}

case class InteractionWithExecutes[S <: Tuple, A] private[dsl](private[dsl] val requires: Seq[Requires[S, A]] = Seq.empty,
                                                      private[dsl] val ensures: Seq[Ensures[S, A]] = Seq.empty,
                                                      private[dsl] val executes: (S, A) => S)
  extends Interaction[S, A] {
  type T[_, _] = InteractionWithExecutes[S, A]

  override inline def requires(inline pred: (S, A) => Boolean): InteractionWithExecutes[S, A] =
    ${ constructInteractionWithExecutesWithRequires('{ this }, '{ pred }) }

  override inline def ensures(inline pred: (S, A) => Boolean): InteractionWithExecutes[S, A] =
    ${ constructInteractionWithExecutesWithEnsures('{ this }, '{ pred }) }

}
