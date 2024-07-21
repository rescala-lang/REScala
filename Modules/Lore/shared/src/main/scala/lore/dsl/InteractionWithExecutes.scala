package lore.dsl

import reactives.SelectedScheduler.State as BundleState
import reactives.core.ReSource
import reactives.operator.Fold.current
import reactives.operator.{Event, Fold, FoldState}

import scala.annotation.targetName
import scala.quoted.{Expr, Quotes, Type}

def constructIWEWithRequires[S <: Tuple, A](
    interaction: Expr[InteractionWithExecutes[S, A]],
    expr: Expr[(S, A) => Boolean]
)(using Quotes, Type[S], Type[A]): Expr[InteractionWithExecutes[S, A]] = '{
  val (inputs, fun, isStatic) =
    reactives.macros.MacroLegos.getDependencies[
      (S, A) => Boolean,
      ReSource.of[BundleState],
      reactives.core.StaticTicket[BundleState],
      true
    ]($expr)

  $interaction.copy(requires = $interaction.requires :+ Requires(inputs, fun, ${ showPredicateCode(expr) }))
}

def constructIWEWithEnsures[S <: Tuple, A](
    interaction: Expr[InteractionWithExecutes[S, A]],
    expr: Expr[(S, A) => Boolean]
)(using Quotes, Type[S], Type[A]): Expr[InteractionWithExecutes[S, A]] = '{
  val (inputs, fun, isStatic) =
    reactives.macros.MacroLegos.getDependencies[
      (S, A) => Boolean,
      ReSource.of[BundleState],
      reactives.core.StaticTicket[BundleState],
      true
    ]($expr)

  $interaction.copy(ensures = $interaction.ensures :+ Ensures(inputs, fun, ${ showPredicateCode(expr) }))
}

case class InteractionWithExecutes[S <: Tuple, A] private[dsl] (
    private[dsl] val requires: Seq[Requires[S, A]] = Seq.empty,
    private[dsl] val ensures: Seq[Ensures[S, A]] = Seq.empty,
    private[dsl] val executes: (S, A) => S
) extends Interaction[S, A] with CanAct[S, A] {
  type T[_, _]  = InteractionWithExecutes[S, A]
  type AO[_, _] = InteractionWithExecutesAndActs[S, A]

  override inline def requires(inline pred: (S, A) => Boolean): InteractionWithExecutes[S, A] =
    ${ constructIWEWithRequires('{ this }, '{ pred }) }

  override inline def ensures(inline pred: (S, A) => Boolean): InteractionWithExecutes[S, A] =
    ${ constructIWEWithEnsures('{ this }, '{ pred }) }

  override def actsOn(event: Event[A]): InteractionWithExecutesAndActs[S, A] =
    InteractionWithExecutesAndActs(requires, ensures, executes, event)

  @targetName("actWithN")
  inline def actWith(inline event: FoldState[S] ?=> Event[A]): Fold.Branch[S] = Fold.branch[S] {
    event.value.fold(current)(executes(current, _))
  }

  @targetName("actWithT1")
  inline def actWith[T1](inline event: FoldState[T1] ?=> Event[A])(using ev: S =:= Tuple1[T1]): Fold.Branch[T1] =
    Fold.branch[T1] {
      event.value.fold(current)(executes(ev.flip.apply(Tuple1(current[T1])), _)._1)
    }

}
