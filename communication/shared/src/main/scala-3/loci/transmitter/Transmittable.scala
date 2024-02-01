package loci
package transmitter

import scala.annotation.{compileTimeOnly, implicitNotFound}
import scala.quoted.*
import scala.util.Try


final class /[D <: Transmittable.Delegating, T <: Transmittable.Any[?, ?, ?]](
  val tail: D,
  val head: T)
    extends Transmittable.Delegating:
  def tailDelegates = new Transmittables.Delegates(tail)


sealed trait Transmittables extends Any

object Transmittables:
  final class Delegates[T <: Transmittable.Delegating](val delegates: T) extends AnyVal with Transmittables
  final class Message[T <: Transmittable.Any[?, ?, ?]](val message: T) extends AnyVal with Transmittables
  final class None extends Transmittables


object Transmittable
    extends TransmittablePrimitives
    with TransmittableTuples
    with TransmittableCollections:

  type Base = Transmittable.type

  sealed trait Delegating

  type Aux[-B, I, +R, P, T <: Transmittables] = Any[B, I, R] {
    type Proxy = P
    type Transmittables = T
  }


  trait Any[-B, I, +R] extends Delegating:
    this: AnyTransmittable[B, I, R] =>

    type Base >: B
    type Intermediate = I
    type Result <: R
    type Proxy
    type Transmittables <: transmitter.Transmittables

    val transmittables: Transmittables

    def buildIntermediate(value: Base)(
      implicit context: Context.Providing[Transmittables]): Intermediate
    def buildResult(value: Intermediate)(
      implicit context: Context.Receiving[Transmittables]): Result
    def buildProxy(value: Notice.Steady[Try[Intermediate]])(
      implicit context: Context.Receiving[Transmittables]): Proxy


  transparent inline def apply[T](using resolution: Resolution[T, ?, ?, ?, ?]) = resolution.transmittable

  transparent inline def Argument[T](using resolution: Resolution[T, ?, T, ?, ?]) = resolution.transmittable


  given nothing: IdenticallyTransmittable[Nothing] = IdenticallyTransmittable()


  sealed trait SurrogateNothing

  object SurrogateNothing {
    @compileTimeOnly("loci.transmitter.transmittable.TransmittableBase.SurrogateNothing is not transmittable")
    implicit def surrogateType[T]: IdenticallyTransmittable[SurrogateNothing] =
      IdenticallyTransmittable()
  }


  @implicitNotFound("${B} is not transmittable")
  final class Resolution[B, I, R, P, T <: Transmittables](
      val value: Transmittable.Aux[B, I, R, P, T]) extends AnyVal:
    type Type = Transmittable.Aux[B, I, R, P, T]
    transparent inline def transmittable: Type = value

  sealed trait ResolutionAlternation:
    given resolutionAlternation[B, I, R](using
      `IsInferred specificity dummy for I`: DummyImplicit,
      `IsInferred specificity dummy for R`: DummyImplicit,
      transmittable: Transmittable.Any[B, I, R])
    : Resolution[B, I, R, transmittable.Proxy, transmittable.Transmittables] =
      Resolution(transmittable)

  sealed trait ResolutionDefault extends ResolutionAlternation:
    given resolution[B, I, R](using
      `IsInferred specificity dummy for I`: DummyImplicit,
      `IsInferred specificity dummy for R`: DummyImplicit,
      transmittable: Transmittable.Any[B, I, R])
    : Resolution[B, I, R, transmittable.Proxy, transmittable.Transmittables] =
      Resolution(transmittable)

  object Resolution extends ResolutionDefault:
    transparent inline given macroGenerated[B, I: IsInferred, R: IsInferred, P, T <: Transmittables](using
      DummyImplicit.Resolvable)
    : Resolution[B, I, R, P, T] =
      ${ TransmittableResolution.optimizedTransmittableResolution[B, I, R, P, T] }


  object Delegating:
    @implicitNotFound("Delegation is not transmittable")
    final class Resolution[D <: Delegating](val transmittables: D) extends AnyVal

    inline given [D <: Delegating]: Resolution[D] =
      ${ TransmittableResolution.delegatingResolution[D] }
end Transmittable
