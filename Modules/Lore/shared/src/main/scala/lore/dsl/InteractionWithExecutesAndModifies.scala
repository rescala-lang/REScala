package lore.dsl

import reactives.core.ReSource
import reactives.operator.Event
import reactives.operator.Interface.State as BundleState

import scala.annotation.static
import scala.quoted.{Expr, Quotes, Type}

def constructIWEAMWithRequires[ST <: Tuple, S <: Tuple, A](interaction: Expr[InteractionWithExecutesAndModifies[ST, S, A]],
                                                                             expr: Expr[(ST, A) => Boolean])
                                                                            (using Quotes, Type[ST], Type[S], Type[A]): Expr[InteractionWithExecutesAndModifies[ST, S, A]] = '{
  val (inputs, fun, isStatic) =
    reactives.macros.MacroLegos.getDependencies[(ST, A) => Boolean, ReSource.of[BundleState], reactives.core.StaticTicket[BundleState], true]($expr)

  $interaction.copy(requires = $interaction.requires :+ Requires(inputs, fun, ${ showPredicateCode(expr) }))
}

def constructIWEAMWithEnsures[ST <: Tuple, S <: Tuple, A](interaction: Expr[InteractionWithExecutesAndModifies[ST, S, A]],
                                                                            expr: Expr[(ST, A) => Boolean])
                                                                           (using Quotes, Type[ST], Type[S], Type[A]): Expr[InteractionWithExecutesAndModifies[ST, S, A]] = '{
  val (inputs, fun, isStatic) =
    reactives.macros.MacroLegos.getDependencies[(ST, A) => Boolean, ReSource.of[BundleState], reactives.core.StaticTicket[BundleState], true]($expr)

  val newEns = Ensures(inputs, fun, ${ showPredicateCode(expr) })

  $interaction.copy(ensures = $interaction.ensures :+ newEns)
}

case class InteractionWithExecutesAndModifies[ST <: Tuple, S <: Tuple, A] private[dsl](
                                                                            private[dsl] val requires: Seq[Requires[ST, A]] = Seq.empty,
                                                                            private[dsl] val ensures: Seq[Ensures[ST, A]] = Seq.empty,
                                                                            private[dsl] val executes: (ST, A) => ST,
                                                                            private[dsl] val modifies: S
                                                                      ) extends Interaction[ST, A] with CanAct[ST, A] {
  type T[_, _] = InteractionWithExecutesAndModifies[ST, S, A]
  type AO[_, _] = BoundInteraction[ST, S, A]

  override inline def requires(inline pred: (ST, A) => Boolean): InteractionWithExecutesAndModifies[ST, S, A] =
    ${ constructIWEAMWithRequires('{ this }, '{ pred }) }

  override inline def ensures(inline pred: (ST, A) => Boolean): InteractionWithExecutesAndModifies[ST, S, A] =
    ${ constructIWEAMWithEnsures('{ this }, '{ pred }) }

  override def actsOn(event: Event[A]): BoundInteraction[ST, S, A] =
    BoundInteraction(requires, ensures, executes, modifies, event)

}
