package loci
package transmitter

import scala.annotation.{compileTimeOnly, implicitNotFound}
import scala.concurrent.Future
import scala.language.experimental.macros
import scala.util.Try


final class /[D <: Transmittable.Delegating, T <: Transmittable.Any[_, _, _]](
    val tail: D, val head: T) extends Transmittable.Delegating {
  def tailDelegates = new Transmittables.Delegates(tail)
}


sealed trait Transmittables extends Any

object Transmittables {
  final class Delegates[T <: Transmittable.Delegating](val delegates: T) extends AnyVal with Transmittables
  final class Message[T <: Transmittable.Any[_, _, _]](val message: T) extends AnyVal with Transmittables
  final class None extends Transmittables
}


object TransmittableBase extends
    TransmittablePrimitives with
    TransmittableTuples with
    TransmittableCollections {

  sealed trait Delegating

  trait Any[-B, I, +R] extends Delegating {
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
  }


  @implicitNotFound("${B} is not transmittable")
  final class Wrapper[B, I, R, P, T <: Transmittables](
      val transmittable: Transmittable.Aux[B, I, R, P, T]) extends AnyVal {
    type Base = B
    type Intermediate = I
    type Result = R
    type Proxy = P
    type Transmittables = T
    type Type = Transmittable.Aux[B, I, R, P, T]
  }

  sealed trait WrapperAlternation {
    implicit def wrapperAlternation[B, I, R, P, T <: Transmittables](implicit
      transmittable: Transmittable.Aux[B, I, R, P, T])
    : Wrapper[B, I, R, P, T] =
      new Wrapper(transmittable)
  }

  object Wrapper extends WrapperAlternation {
    implicit def wrapper[B, I, R, P, T <: Transmittables](implicit
      transmittable: Transmittable.Aux[B, I, R, P, T])
    : Wrapper[B, I, R, P, T] =
      new Wrapper(transmittable)
  }


  implicit def nothing: IdenticallyTransmittable[Nothing] =
    IdenticallyTransmittable()


  sealed trait SurrogateType[T, U, V]

  object SurrogateType {
    @compileTimeOnly("loci.transmitter.transmittable.TransmittableBase.SurrogateType is not transmittable")
    implicit def surrogateType[T, V]: IdenticallyTransmittable[SurrogateType[T, Nothing, V]] =
      IdenticallyTransmittable()
  }


  @implicitNotFound("${B} is not transmittable")
  final class DependantValue[B, I, R, +V] private (val value: V) extends AnyVal

  object DependantValue {
    implicit def dependantValue[B, I, R, P, T <: Transmittables](implicit
      wrapper: Wrapper[B, I, R, P, T])
    : DependantValue[B, I, R, wrapper.Type] =
      new DependantValue(wrapper.transmittable)
  }


  @implicitNotFound("${B} is not transmittable")
  final class Resolution[B, I, R, P, T <: Transmittables](
      val value: Transmittable.Aux[B, I, R, P, T]) extends AnyVal {
    type Type = Transmittable.Aux[B, I, R, P, T]
    def transmittable: Type = value
  }

  sealed trait ResolutionFailure {
    @compileTimeOnly("Value is not transmittable")
    implicit def resolutionFailure[B, I, R, P, T <: Transmittables](implicit
      dummy: DummyImplicit.Unresolvable)
    : Resolution[B, I, R, P, T] = {
      locally(dummy)
      throw new NotImplementedError
    }
  }

  sealed trait ResolutionDefault extends ResolutionFailure {
    implicit def default[B, I, R, P, T <: Transmittables](implicit
      dependant: DependantValue[B, I, R, Transmittable.Aux[B, I, R, P, T]])
    : Resolution[B, I, R, P, T] =
      new Resolution(dependant.value)
  }

  sealed trait ResolutionNothing extends ResolutionDefault {
    implicit def nothing(implicit
      dummy: DummyImplicit.Unresolvable)
    : Resolution[Nothing, Nothing, Nothing, Future[Nothing], Transmittables.None] ={
      locally(dummy)
      throw new NotImplementedError
    }
  }

  object Resolution extends ResolutionNothing {
    implicit def macroGenerated[B, I, R, P, T <: Transmittables](implicit
      dummy: DummyImplicit.Resolvable)
    : Resolution[B, I, R, P, T] =
      macro TransmittableResolution[B, I, R, P, T]
  }


  sealed trait DelegatingFailure {
    @compileTimeOnly("Delegation is not transmittable")
    implicit def resolutionFailure[D <: Delegating](implicit
      dummy: DummyImplicit.Unresolvable)
    : Delegating.Resolution[D] = {
      locally(dummy)
      throw new NotImplementedError
    }
  }

  object Delegating extends DelegatingFailure {
    final class Resolution[D <: Delegating](
      val transmittables: D) extends AnyVal

    implicit def single[B, I, R, P, T <: Transmittables](implicit
      resolution: Transmittable.Resolution[B, I, R, P, T])
    : Resolution[Transmittable.Aux[B, I, R, P, T]] =
      new Resolution(resolution.transmittable)

    implicit def list[B, I, R, P, T <: Transmittables, D <: Delegating](implicit
      resolution: Transmittable.Resolution[B, I, R, P, T],
      delegates: Resolution[D])
    : Resolution[D / Transmittable.Aux[B, I, R, P, T]] =
      new Resolution(new / (delegates.transmittables, resolution.transmittable))
  }
}
