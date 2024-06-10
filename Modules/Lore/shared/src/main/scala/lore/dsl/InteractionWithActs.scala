package lore.dsl

import reactives.core.ReSource
import reactives.operator.Event
import reactives.SelectedScheduler.State as BundleState

import scala.annotation.static
import scala.quoted.{Expr, Quotes, Type}

def constructIWAWithRequires[S <: Tuple, A](
    interaction: Expr[InteractionWithActs[S, A]],
    expr: Expr[(S, A) => Boolean]
)(using Quotes, Type[S], Type[A]): Expr[InteractionWithActs[S, A]] = '{
  val (inputs, fun, isStatic) =
    reactives.macros.MacroLegos.getDependencies[
      (S, A) => Boolean,
      ReSource.of[BundleState],
      reactives.core.StaticTicket[BundleState],
      true
    ]($expr)

  $interaction.copy(requires = $interaction.requires :+ Requires(inputs, fun, ${ showPredicateCode(expr) }))
}

def constructIWAWithEnsures[S <: Tuple, A](
    interaction: Expr[InteractionWithActs[S, A]],
    expr: Expr[(S, A) => Boolean]
)(using Quotes, Type[S], Type[A]): Expr[InteractionWithActs[S, A]] = '{
  val (inputs, fun, isStatic) =
    reactives.macros.MacroLegos.getDependencies[
      (S, A) => Boolean,
      ReSource.of[BundleState],
      reactives.core.StaticTicket[BundleState],
      true
    ]($expr)

  $interaction.copy(ensures = $interaction.ensures :+ Ensures(inputs, fun, ${ showPredicateCode(expr) }))
}

case class InteractionWithActs[S <: Tuple, A] private[dsl] (
    private[dsl] val requires: Seq[Requires[S, A]] = Seq.empty,
    private[dsl] val ensures: Seq[Ensures[S, A]] = Seq.empty,
    private[dsl] val event: Event[A]
) extends Interaction[S, A] with CanExecute[S, A] {
  type T[_, _] = InteractionWithActs[S, A]
  type E[_, _] = InteractionWithExecutesAndActs[S, A]

  override inline def requires(inline pred: (S, A) => Boolean): InteractionWithActs[S, A] =
    ${ constructIWAWithRequires('{ this }, '{ pred }) }

  override inline def ensures(inline pred: (S, A) => Boolean): InteractionWithActs[S, A] =
    ${ constructIWAWithEnsures('{ this }, '{ pred }) }

  override def executes(fun: (S, A) => S): InteractionWithExecutesAndActs[S, A] =
    InteractionWithExecutesAndActs(requires, ensures, fun, event)

}
