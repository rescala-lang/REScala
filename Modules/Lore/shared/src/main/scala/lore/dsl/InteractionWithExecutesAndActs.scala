package lore.dsl

import reactives.core.ReSource
import reactives.operator.Event
import reactives.operator.Interface.State as BundleState

import scala.annotation.static
import scala.quoted.{Expr, Quotes, Type}

def constructIWEAAWithRequires[S <: Tuple, A](
    interaction: Expr[InteractionWithExecutesAndActs[S, A]],
    expr: Expr[(S, A) => Boolean]
)(using Quotes, Type[S], Type[A]): Expr[InteractionWithExecutesAndActs[S, A]] = '{
  val (inputs, fun, isStatic) =
    reactives.macros.MacroLegos.getDependencies[
      (S, A) => Boolean,
      ReSource.of[BundleState],
      reactives.core.StaticTicket[BundleState],
      true
    ]($expr)

  $interaction.copy(requires = $interaction.requires :+ Requires(inputs, fun, ${ showPredicateCode(expr) }))
}

def constructIWEAAWithEnsures[S <: Tuple, A](
    interaction: Expr[InteractionWithExecutesAndActs[S, A]],
    expr: Expr[(S, A) => Boolean]
)(using Quotes, Type[S], Type[A]): Expr[InteractionWithExecutesAndActs[S, A]] = '{
  val (inputs, fun, isStatic) =
    reactives.macros.MacroLegos.getDependencies[
      (S, A) => Boolean,
      ReSource.of[BundleState],
      reactives.core.StaticTicket[BundleState],
      true
    ]($expr)

  $interaction.copy(ensures = $interaction.ensures :+ Ensures(inputs, fun, ${ showPredicateCode(expr) }))
}

case class InteractionWithExecutesAndActs[S <: Tuple, A] private[dsl] (
    private[dsl] val requires: Seq[Requires[S, A]] = Seq.empty,
    private[dsl] val ensures: Seq[Ensures[S, A]] = Seq.empty,
    private[dsl] val executes: (S, A) => S,
    private[dsl] val event: Event[A]
) extends Interaction[S, A] {
  type T[_, _] = InteractionWithExecutesAndActs[S, A]

  override inline def requires(inline pred: (S, A) => Boolean): InteractionWithExecutesAndActs[S, A] =
    ${ constructIWEAAWithRequires('{ this }, '{ pred }) }

  override inline def ensures(inline pred: (S, A) => Boolean): InteractionWithExecutesAndActs[S, A] =
    ${ constructIWEAAWithEnsures('{ this }, '{ pred }) }

}
