package lore.dsl

import scala.quoted.{Expr, Quotes, Type}

def constructBoundInteractionWithRequires[S, A](requires: Expr[Seq[Requires[S, A]]],
                                                ensures: Expr[Seq[Ensures[S, A]]],
                                                executes: Expr[(S, A) => S],
                                                modifies: Expr[Source[S]],
                                                expr: Expr[(S, A) => Boolean],
                                                invariantManager: Expr[InvariantManager])
                                               (using Quotes, Type[S], Type[A]): Expr[BoundInteraction[S, A]] =
  '{ BoundInteraction[S, A]($requires :+ Requires($expr, ${ showPredicateCode(expr) }), $ensures, $executes, $modifies, $invariantManager) }

def constructBoundInteractionWithEnsures[S, A](requires: Expr[Seq[Requires[S, A]]],
                                               ensures: Expr[Seq[Ensures[S, A]]],
                                               executes: Expr[(S, A) => S],
                                               modifies: Expr[Source[S]],
                                               expr: Expr[(S, A) => Boolean],
                                               invariantManager: Expr[InvariantManager])
                                              (using Quotes, Type[S], Type[A]): Expr[BoundInteraction[S, A]] =
  '{ BoundInteraction[S, A]($requires, $ensures :+ Ensures($expr, ${ showPredicateCode(expr) }), $executes, $modifies, $invariantManager) }

case class BoundInteraction[S, A] private[dsl](private[dsl] val requires: Seq[Requires[S, A]] = Seq.empty,
                                               private[dsl] val ensures: Seq[Ensures[S, A]] = Seq.empty,
                                               private[dsl] val executes: (S, A) => S,
                                               private[dsl] val modifies: Source[S],
                                               private[dsl] val invariantManager: InvariantManager)
  extends Interaction[S, A]
    with CanApply[S, A] {
  type T[_, _] = BoundInteraction[S, A]

  override inline def requires(inline pred: (S, A) => Boolean): BoundInteraction[S, A] =
    ${ constructBoundInteractionWithRequires('requires, 'ensures, 'executes, 'modifies, 'pred, 'invariantManager) }

  override inline def ensures(inline pred: (S, A) => Boolean): BoundInteraction[S, A] =
    ${ constructBoundInteractionWithEnsures('requires, 'ensures, 'executes, 'modifies, 'pred, 'invariantManager) }

  private def checkRequires(currentValue: S, arg: A): Boolean = {
    var success = true

    for req <- requires do {
      if !req.predicate(currentValue, arg) then {
        success = false
        println(s"Interaction violated requirement: ${req.representation} evaluated to false!")
      }
    }

    success
  }

  private def checkEnsures(currentValue: S, arg: A): Boolean = {
    var success = true

    for ens <- ensures do {
      if !ens.predicate(currentValue, arg) then {
        success = false
        // TODO (Svenja, 24.02.2024): Is there another word for post-condition which is closer to ensures like requires and requirement?
        println(s"Interaction violated post-condition: ${ens.representation} evaluated to false!")
      }
    }

    success
  }

  override def apply(arg: A): Unit = {
    modifies.transform(currVal =>
      if !checkRequires(currVal, arg) || !invariantManager.checkInvariants() then currVal else {
        val res = executes(currVal, arg)
        if !checkEnsures(res, arg) || !invariantManager.checkInvariants() then currVal else res
      }
    )
  }
}
