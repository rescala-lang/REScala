package lore.dsl

import reactives.core.ReSource
import reactives.operator.Fold.current
import reactives.operator.{Event, Fold, FoldState}
import reactives.SelectedScheduler.State as BundleState

import scala.annotation.{static, targetName}
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

  @targetName("foldIntoN")
  inline def foldInto(): Fold.Branch[S] =
    event.branch { arg =>
      executes(current, arg)
    }

  @targetName("foldIntoT1")
  inline def foldInto[T](using ev: S =:= Tuple1[T]): Fold.Branch[T] =
    event.branch { arg =>
      executes(ev.flip.apply(Tuple1(current[T])), arg)._1
    }

  @targetName("foldIntoBranchN")
  inline def foldIntoBranch(using FoldState[S]): S =
    event.value.fold(current)(executes(current, _))

  @targetName("foldIntoBranchT1")
  inline def foldIntoBranch[T](using FoldState[T])(using ev: S =:= Tuple1[T]): T =
    event.value.fold(current)(executes(ev.flip.apply(Tuple1(current[T])), _)._1)

}
