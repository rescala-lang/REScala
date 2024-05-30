package lore.dsl

import reactives.core.ReSource
import reactives.default.*
import reactives.operator.Interface.State as BundleState


import scala.quoted.{Expr, Quotes, Type}
import scala.reflect.ClassTag

def constructBoundInteractionWithRequires[ST <: Tuple, S <: Tuple, A](interaction: Expr[BoundInteraction[ST, S, A]],
                                                                      expr: Expr[(ST, A) => Boolean])
                                                                     (using Quotes, Type[ST], Type[S], Type[A]): Expr[BoundInteraction[ST, S, A]] = '{
  val (inputs, fun, isStatic) =
    reactives.macros.MacroLegos.getDependencies[(ST, A) => Boolean, ReSource.of[BundleState], reactives.core.StaticTicket[BundleState], true]($expr)

  $interaction.copy(requires = $interaction.requires :+ Requires(inputs, fun, ${ showPredicateCode(expr) }))
}

def constructBoundInteractionWithEnsures[ST <: Tuple, S <: Tuple, A](interaction: Expr[BoundInteraction[ST, S, A]],
                                                                     expr: Expr[(ST, A) => Boolean])
                                                                    (using Quotes, Type[ST], Type[S], Type[A]): Expr[BoundInteraction[ST, S, A]] = '{
  val (inputs, fun, isStatic) =
    reactives.macros.MacroLegos.getDependencies[(ST, A) => Boolean, ReSource.of[BundleState], reactives.core.StaticTicket[BundleState], true]($expr)

  $interaction.copy(ensures = $interaction.ensures :+ Ensures(inputs, fun, ${ showPredicateCode(expr) }))
}

case class BoundInteraction[ST <: Tuple, S <: Tuple, A] private[dsl](private[dsl] val requires: Seq[Requires[ST, A]] = Seq.empty,
                                                                     private[dsl] val ensures: Seq[Ensures[ST, A]] = Seq.empty,
                                                                     private[dsl] val executes: (ST, A) => ST,
                                                                     private[dsl] val modifies: S,
                                                                     private[dsl] val event: Event[A])
  extends Interaction[ST, A] {

  type T[_, _] = BoundInteraction[ST, S, A]

  event.observe { it => apply(it) }

  override inline def requires(inline pred: (ST, A) => Boolean): BoundInteraction[ST, S, A] =
    ${ constructBoundInteractionWithRequires('{ this }, '{ pred }) }

  override inline def ensures(inline pred: (ST, A) => Boolean): BoundInteraction[ST, S, A] =
    ${ constructBoundInteractionWithEnsures('{ this }, '{ pred }) }

  def apply(a: A): Unit = {
    val modList = modifies.toList.asInstanceOf[List[Var[?]]]
    transaction((modList ++ requires.flatMap(_.inputs) ++ ensures.flatMap(_.inputs)).asInstanceOf[Seq[Var[?]]] *) { implicit at =>
      val curr = modList.map(it => at.now(it))

      val t = Tuple.fromArray(curr.toArray).asInstanceOf[ST]

      for req <- requires do {
        if !req.fun(at.tx.preconditionTicket)(t, a) then {
          val message = s"Interaction violated requirement: ${req.representation} with argument ($curr, $a) evaluated to false!"
          throw new IllegalStateException(message)
        }
      }

      val res = executes(t, a)

      for ens <- ensures do {
        if !ens.fun(at.tx.preconditionTicket)(res, a) then {
          val message = s"Interaction violated post-condition: ${ens.representation} with argument (($curr), $a) evaluated to false!"
          throw new IllegalStateException(message)
        }
      }

      modifies.zip(res).toList.asInstanceOf[Seq[(Var[Any], Any)]].foreach { case (source, v) => source.set(v) }
    }

  }

}

