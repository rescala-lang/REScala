package lore.dsl

import reactives.core.ReSource
import reactives.operator.Event
import reactives.operator.Interface.State as BundleState

import scala.annotation.static
import scala.quoted.{Expr, Quotes, Type}

def constructIWMAAWithRequires[ST <: Tuple, S <: Tuple, A](interaction: Expr[InteractionWithModifiesAndActs[ST, S, A]],
                                                                             expr: Expr[(ST, A) => Boolean])
                                                                            (using Quotes, Type[ST], Type[S], Type[A]): Expr[InteractionWithModifiesAndActs[ST, S, A]] = '{
  val (inputs, fun, isStatic) =
    reactives.macros.MacroLegos.getDependencies[(ST, A) => Boolean, ReSource.of[BundleState], reactives.core.StaticTicket[BundleState], true]($expr)

  $interaction.copy(requires = $interaction.requires :+ Requires(inputs, fun, ${ showPredicateCode(expr) }))
}

def constructIWMAAWithEnsures[ST <: Tuple, S <: Tuple, A](interaction: Expr[InteractionWithModifiesAndActs[ST, S, A]],
                                                                            expr: Expr[(ST, A) => Boolean])
                                                                           (using Quotes, Type[ST], Type[S], Type[A]): Expr[InteractionWithModifiesAndActs[ST, S, A]] = '{
  val (inputs, fun, isStatic) =
    reactives.macros.MacroLegos.getDependencies[(ST, A) => Boolean, ReSource.of[BundleState], reactives.core.StaticTicket[BundleState], true]($expr)

  val newEns = Ensures(inputs, fun, ${ showPredicateCode(expr) })

  $interaction.copy(ensures = $interaction.ensures :+ newEns)
}

case class InteractionWithModifiesAndActs[ST <: Tuple, S <: Tuple, A] private[dsl](
                                                                        private[dsl] val requires: Seq[Requires[ST, A]] = Seq.empty,
                                                                        private[dsl] val ensures: Seq[Ensures[ST, A]] = Seq.empty,
                                                                        private[dsl] val modifies: S,
                                                                        private[dsl] val event: Event[A]
                                                                     ) extends Interaction[ST, A] with CanExecute[ST, A] {
  type T[_, _] = InteractionWithModifiesAndActs[ST, S, A]
  type E[_, _] = BoundInteraction[ST, S, A]

  override inline def requires(inline pred: (ST, A) => Boolean): InteractionWithModifiesAndActs[ST, S, A] =
    ${ constructIWMAAWithRequires('{ this }, '{ pred }) }

  override inline def ensures(inline pred: (ST, A) => Boolean): InteractionWithModifiesAndActs[ST, S, A] =
    ${ constructIWMAAWithEnsures('{ this }, '{ pred }) }


  override def executes(fun: (ST, A) => ST): BoundInteraction[ST, S, A] =
    BoundInteraction(requires, ensures, fun, modifies, event)

}
