package lore.DSL
import scala.quoted.*
import rescala.default.*
import lore.Parser

type Source[A] = Var[A]
type Derived[A] = Signal[A]

object Source:
  inline def apply[A](inline expr: A): Var[A] = Var(expr)

object Derived:
  inline def apply[A](inline expr: A): Signal[A] = Signal { expr }

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
private class InteractionWithTypes[S, A]:
  inline def requires(inline expr: (S, A) => Boolean): InteractionWithRequires[S, A] = InteractionWithRequires(
    expr
  )

private class InteractionWithRequires[S, A](requires: (S, A) => Boolean):
  inline def modifies(inline expr: Source[S]): InteractionWithRequiresAndModifies[S, A] =
    InteractionWithRequiresAndModifies(requires, expr)

// private class InteractionWithModifies[A](modifies: Source[A]):
//   inline def executes(inline expr: A => A) =
//     () => modifies.transform(expr)
//   inline def requires(inline expr: () => Boolean) =
//     InteractionWithRequiresAndModifies(expr, modifies)

private class InteractionWithRequiresAndModifies[S, A](
    requires: (S, A) => Boolean,
    modifies: Source[S]
):
  inline def executes(inline expr: (S, A) => S): A => Unit =
    (arg: A) =>
      modifies.transform(currVal =>
        if requires(currVal, arg) then expr(currVal, arg)
        else
          println(s"Requirement $requires evaluated to false!")
          currVal
      )

object Interaction:
  inline def apply[S, A]: InteractionWithTypes[S, A] = InteractionWithTypes[S, A]
  // inline def modifies[A](source: Source[A]) = InteractionWithModifies(source)
  inline def requires[S, A](b: Boolean): Boolean =
    ${
      makeFromRequires('b)
    }

def makeFromRequires(b: Expr[Boolean])(using Quotes) =
  println(b.show)
  Parser.parse(b.toString) match
    case Left(err)    => println(err)
    case Right(value) => println(value)
  b
