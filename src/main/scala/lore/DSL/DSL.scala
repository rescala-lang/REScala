package lore.DSL

import scala.quoted.*
import rescala.default.*
import lore.Parser

type Source[A] = Var[A]
type Derived[A] = Signal[A]

object Source {
  inline def apply[A](inline expr: A): Var[A] = Var(expr)
}

object Derived {
  inline def apply[A](inline expr: A): Signal[A] = Signal {
    expr
  }
}

// S = source type, A = argument type
// case class Interaction(
//     req: List[String],
//     ens: List[Boolean],
//     exec: List[Any]
// )
// class InteractionWithModifiesAndExecutes[A](
//     modifies: List[Source[A]],
//     executes: () => List[A]
// )
private class InteractionWithTypes[S, A] {
  inline def requires(inline expr: (S, A) => Boolean): InteractionWithRequires[S, A] = InteractionWithRequires(
    expr
  )
}

private class InteractionWithRequires[S, A](requires: (S, A) => Boolean) {
  inline def modifies(inline expr: Source[S]): InteractionWithRequiresAndModifies[S, A] =
    InteractionWithRequiresAndModifies(requires, expr)
}

// private class InteractionWithModifies[A](modifies: Source[A]):
//   inline def executes(inline expr: A => A) =
//     () => modifies.transform(expr)
//   inline def requires(inline expr: () => Boolean) =
//     InteractionWithRequiresAndModifies(expr, modifies)

private class InteractionWithRequiresAndModifies[S, A](
                                                        requires: (S, A) => Boolean,
                                                        modifies: Source[S]
                                                      ) {
  inline def executes(inline expr: (S, A) => S)(using invariantManager: InvariantManager): A => Unit =
    (arg: A) =>
      modifies.transform(currVal =>
        if !requires(currVal, arg) then {
          println(s"Requirement $requires evaluated to false!")
          currVal
        }
        else
          if !invariantManager.checkInvariants() then {
            print(s"Invariants pre failed")
            currVal
          }
          else {
            val res = expr(currVal, arg)

            if !invariantManager.checkInvariants() then {
              print("Invariants post failed")
              currVal
            }
            else
              res
          }

      )
}

trait Invariant {

  def apply(): Boolean

}

object Invariant {
  inline def apply(name: String)(using invariantManager: InvariantManager): Invariant = {
    val invariant = new Invariant {

      override def apply(): Boolean = {
        println(f"Checking invariant $name")

        true
      }

    }

    invariantManager.registerInvariant(invariant)

    invariant
  }
}

trait InvariantManager {

  def registerInvariant(invariant: Invariant): Unit

  def registeredInvariants: Seq[Invariant]

  def checkInvariants(): Boolean = {
    registeredInvariants.forall(_.apply())
  }

}

given defaultInvariantManager: InvariantManager = new InvariantManager {
  private var invariants: List[Invariant] = List.empty
  
  override def registerInvariant(invariant: Invariant): Unit = invariants :+= invariant

  override def registeredInvariants: Seq[Invariant] = invariants
}

object Interaction {
  inline def apply[S, A](using invariantManager: InvariantManager): InteractionWithTypes[S, A] = InteractionWithTypes[S, A]

  // inline def modifies[A](source: Source[A]) = InteractionWithModifies(source)
  inline def requires[S, A](b: Boolean): Boolean =
    ${
      makeFromRequires('b)
    }
}

def makeFromRequires(b: Expr[Boolean])(using Quotes) = {
  println(b.show)
  Parser.parse(b.toString) match {
    case Left(err) => println(err)
    case Right(value) => println(value)
  }
  b
}
