package loci
package transmitter

import Selector.*

import scala.compiletime.ops.int
import scala.quoted.*
import scala.runtime.Tuples

object GenericTuple:
  sealed trait IdenticalDelegation[T <: Tuple]

  object IdenticalDelegation:
    given empty: IdenticalDelegation[EmptyTuple]()
    given nonempty[H: IdenticallyTransmittable, T <: Tuple: IdenticalDelegation]: IdenticalDelegation[H *: T]()

  final class Delegation[
    B <: NonEmptyTuple, I <: NonEmptyTuple, R <: NonEmptyTuple,
    D <: Transmittable.Delegating, Outer <: Boolean](
      val delegating: D,
      val provide: (B, DelegatingTransmittable.ProvidingContext[D]) => I,
      val receive: (I, DelegatingTransmittable.ReceivingContext[D]) => R):
    type Delegating = D

  sealed trait MultipleElementsDelegation:
    transparent inline given multiple[
        B, I, R, P, T <: Transmittables,
        BT <: NonEmptyTuple, IT <: NonEmptyTuple, RT <: NonEmptyTuple,
        D <: Transmittable.Delegating, Outer <: Boolean](using
      inline resolution: Transmittable.Resolution[B, I, R, P, T],
      inline delegation: Delegation[BT, IT, RT, D, false])
    : Delegation[B *: BT, I *: IT, R *: RT, D / Transmittable.Aux[B, I, R, P, T], Outer] =
      ${ Delegation.multipleImpl('resolution, 'delegation) }

    def multipleImpl[
        B: Type, I: Type, R: Type, P: Type, T <: Transmittables: Type,
        BT <: NonEmptyTuple: Type, IT <: NonEmptyTuple: Type, RT <: NonEmptyTuple: Type,
        D <: Transmittable.Delegating: Type, Outer <: Boolean: Type](
      resolution: Expr[Transmittable.Resolution[B, I, R, P, T]],
      delegation: Expr[Delegation[BT, IT, RT, D, false]])(using Quotes)
    : Expr[Delegation[B *: BT, I *: IT, R *: RT, D / Transmittable.Aux[B, I, R, P, T], Outer]] =
      import quotes.reflect.*

      val transmittable = resolution match
        case '{ Transmittable.Resolution[B, I, R, P, T]($transmittable) } => transmittable
        case '{ Transmittable.Resolution.resolution[B, I, R](using $i, $b, $transmittable) } => transmittable
        case '{ Transmittable.Resolution.resolutionAlternation[B, I, R](using $i, $b, $transmittable) } => transmittable

      val Block(_, expr) = delegation.asTerm.underlyingArgument: @unchecked
      val '{ new Delegation[BT, IT, RT, D, false]($delegates, $provide, $receive) } = expr.asExpr: @unchecked

      val (provideTransformation, receiveTransformation) =
        makeTransformations[B *: BT, I *: IT, R *: RT, Outer]

      '{
        import scala.language.unsafeNulls
        Delegation(
          /($delegates, ${transmittable.asExprOf[Transmittable.Aux[B, I, R, P, T]]}),
          $provideTransformation,
          $receiveTransformation)
      }
    end multipleImpl
  end MultipleElementsDelegation

  object Delegation extends MultipleElementsDelegation:
    transparent inline given single[B, I, R, P, T <: Transmittables, Outer <: Boolean](using
      inline resolution: Transmittable.Resolution[B, I, R, P, T])
    : Delegation[B *: EmptyTuple, I *: EmptyTuple, R *: EmptyTuple, Transmittable.Aux[B, I, R, P, T], Outer] =
      ${ Delegation.singleImpl('resolution) }

    def singleImpl[B: Type, I: Type, R: Type, P: Type, T <: Transmittables: Type, Outer <: Boolean: Type](
      resolution: Expr[Transmittable.Resolution[B, I, R, P, T]])(using Quotes)
    : Expr[Delegation[B *: EmptyTuple, I *: EmptyTuple, R *: EmptyTuple, Transmittable.Aux[B, I, R, P, T], Outer]] =
      import quotes.reflect.*

      val transmittable = resolution match
        case '{ Transmittable.Resolution[B, I, R, P, T]($transmittable) } => transmittable
        case '{ Transmittable.Resolution.resolution[B, I, R](using $i, $b, $transmittable) } => transmittable
        case '{ Transmittable.Resolution.resolutionAlternation[B, I, R](using $i, $b, $transmittable) } => transmittable

      val (provideTransformation, receiveTransformation) =
        makeTransformations[B *: EmptyTuple, I *: EmptyTuple, R *: EmptyTuple, Outer]

      '{
        import scala.language.unsafeNulls
        Delegation(
          ${transmittable.asExprOf[Transmittable.Aux[B, I, R, P, T]]},
          $provideTransformation,
          $receiveTransformation)
      }
    end singleImpl
  end Delegation

  private def makeTransformations[
      B <: Tuple: Type, I <: Tuple: Type, R <: Tuple: Type, Outer <: Boolean: Type](using Quotes) =
    import quotes.reflect.*

    if Type.valueOfConstant[Outer] contains true then
      import scala.language.unsafeNulls

      val ConstantType(IntConstant(size)) = TypeRepr.of[Tuple.Size[B]].simplified: @unchecked

      val provideTransformation = '{
        (value: Product, context: DelegatingTransmittable.ProvidingContext[?]) => {
          if value == null then
            null
          else
            Tuples.fromIArray(IArray.tabulate[Object](${Expr(size)}) { i =>
              (context delegate value.productElement(i))(using Base(using null, ValueOf(i)))
            })
        }.asInstanceOf[I]
      }

      val receiveTransformation = '{
        (value: Product, context: DelegatingTransmittable.ReceivingContext[?]) => {
          if value == null then
            null
          else
            Tuples.fromIArray(IArray.tabulate[Object](${Expr(size)}) { i =>
              (context delegate value.productElement(i))(using Intermediate(using null, ValueOf(i)))
            })
        }.asInstanceOf[R]
      }

      (provideTransformation, receiveTransformation)
    else
      ('{ ??? }, '{ ??? })
  end makeTransformations
end GenericTuple

trait TransmittableGeneralGenericTuples extends TransmittableDummy:
  this: Transmittable.Base =>

  given tuple[B <: NonEmptyTuple, I <: NonEmptyTuple, R <: NonEmptyTuple, D <: Transmittable.Delegating]
    (using delegation: GenericTuple.Delegation[B, I, R, D, true])
  : (DelegatingTransmittable[B, I, R] {
      type Delegates = delegation.Delegating
    }) =
    DelegatingTransmittable(
      delegation.provide,
      delegation.receive)(using
      Transmittable.Delegating.Resolution(delegation.delegating))

trait TransmittableGenericTuples extends TransmittableGeneralGenericTuples:
  this: Transmittable.Base =>

  given identicalTuple[T <: Tuple: GenericTuple.IdenticalDelegation]
  : IdenticallyTransmittable[T] = IdenticallyTransmittable()
