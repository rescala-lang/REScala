package lore.dsl

import reactives.core.ReSource
import reactives.operator.Interface.State as BundleState


import scala.quoted.{Expr, Quotes, Type}

def constructUnboundInteractionWithRequires[S <: Tuple, A](interaction: Expr[UnboundInteraction[S, A]],
                                                  expr: Expr[(S, A) => Boolean])
                                                 (using Quotes, Type[S], Type[A]): Expr[UnboundInteraction[S, A]] = '{
  val (inputs, fun, isStatic) =
    reactives.macros.MacroLegos.getDependencies[(S, A) => Boolean, ReSource.of[BundleState], reactives.core.StaticTicket[BundleState], true]($expr)

  $interaction.copy(requires = $interaction.requires :+ Requires(inputs, fun, ${ showPredicateCode(expr) }))
}

def constructUnboundInteractionWithEnsures[S <: Tuple, A](interaction: Expr[UnboundInteraction[S, A]],
                                                 expr: Expr[(S, A) => Boolean])
                                                (using Quotes, Type[S], Type[A]): Expr[UnboundInteraction[S, A]] = '{
  val (inputs, fun, isStatic) =
    reactives.macros.MacroLegos.getDependencies[(S, A) => Boolean, ReSource.of[BundleState], reactives.core.StaticTicket[BundleState], true]($expr)

  $interaction.copy(ensures = $interaction.ensures :+ Ensures(inputs, fun, ${ showPredicateCode(expr) }))
}

case class UnboundInteraction[S <: Tuple, A] private[dsl](private[dsl] val requires: Seq[Requires[S, A]] = Seq.empty,
                                                 private[dsl] val ensures: Seq[Ensures[S, A]] = Seq.empty)
  extends Interaction[S, A] with CanExecute[S, A] {
  type T[_, _] = UnboundInteraction[S, A]
  type E[_, _] = InteractionWithExecutes[S, A]

  override inline def requires(inline pred: (S, A) => Boolean): UnboundInteraction[S, A] =
    ${ constructUnboundInteractionWithRequires('{ this }, '{ pred }) }

  override inline def ensures(inline pred: (S, A) => Boolean): UnboundInteraction[S, A] =
    ${ constructUnboundInteractionWithEnsures('{ this }, '{ pred }) }

  override def executes(fun: (S, A) => S): InteractionWithExecutes[S, A] =
    InteractionWithExecutes(requires, ensures, fun)
}
