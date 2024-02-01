package loci
package transmitter

import contexts.Immediate.Implicits.global

import scala.concurrent.{Future, Promise}
import scala.reflect.ClassTag
import scala.util.{Failure, Success}

trait TransmittableGeneralCollections
    extends TransmittableGeneralIterableCollections
    with TransmittableDummy {
  this: Transmittable.Base =>

  final implicit def array[B: ClassTag, I: ClassTag, R: ClassTag]
    (implicit transmittable: Transmittable[B, I, R])
  : DelegatingTransmittable[Array[B], Array[I], Array[R]] {
      type Delegates = transmittable.Type
    } =
    DelegatingTransmittable(
      provide = (value, context) =>
        if (value == null) null else value map { context delegate _ },
      receive = (value, context) =>
        if (value == null) null else value map { context delegate _ })

  final implicit def map[KB, KI, KR, VB, VI, VR]
    (implicit
        transmittableKey: Transmittable[KB, KI, KR],
        transmittableValue: Transmittable[VB, VI, VR])
  : DelegatingTransmittable[Map[KB, VB], Map[KI, VI], Map[KR, VR]] {
      type Delegates = transmittableKey.Type / transmittableValue.Type
    } =
    DelegatingTransmittable(
      provide = (value, context) =>
        if (value == null) null else value map { case (key, value) =>
          (context delegate key, context delegate value) },
      receive = (value, context) =>
        if (value == null) null else value map { case (key, value) =>
          (context delegate key, context delegate value) })

  final implicit def option[B, I, R]
    (implicit transmittable: Transmittable[B, I, R])
  : DelegatingTransmittable[Option[B], Option[I], Option[R]] {
      type Delegates = transmittable.Type
    } =
    DelegatingTransmittable(
      provide = (value, context) =>
        if (value == null) null else value map { context delegate _ },
      receive = (value, context) =>
        if (value == null) null else value map { context delegate _ })

  final implicit def some[B, I, R]
    (implicit transmittable: Transmittable[B, I, R])
  : DelegatingTransmittable[Some[B], Some[I], Some[R]] {
      type Delegates = transmittable.Type
    } =
    DelegatingTransmittable(
      provide = (value, context) =>
        if (value == null) null else Some(context delegate value.get),
      receive = (value, context) =>
        if (value == null) null else Some(context delegate value.get))

  final implicit def either[LB, LI, LR, RB, RI, RR]
    (implicit
        transmittableLeft: Transmittable[LB, LI, LR],
        transmittableRight: Transmittable[RB, RI, RR])
  : DelegatingTransmittable[Either[LB, RB], Either[LI, RI], Either[LR, RR]] {
      type Delegates = transmittableLeft.Type / transmittableRight.Type
    } =
    DelegatingTransmittable(
      provide = (value, context) => value match {
        case null => null
        case Left(value) => Left(context delegate value)
        case Right(value) => Right(context delegate value)
      },
      receive = (value, context) => value match {
        case null => null
        case Left(value) => Left(context delegate value)
        case Right(value) => Right(context delegate value)
      })

  final implicit def left[LB, LI, LR, RB, RI, RR]
    (implicit transmittable: Transmittable[LB, LI, LR])
  : DelegatingTransmittable[Left[LB, RB], Left[LI, RI], Left[LR, RR]] {
      type Delegates = transmittable.Type
    } =
    DelegatingTransmittable(
      provide = (value, context) =>
        if (value == null) null else Left(context delegate compatibility.either.left(value)),
      receive = (value, context) =>
        if (value == null) null else Left(context delegate compatibility.either.left(value)))

  final implicit def right[LB, LI, LR, RB, RI, RR]
    (implicit transmittable: Transmittable[RB, RI, RR])
  : DelegatingTransmittable[Right[LB, RB], Right[LI, RI], Right[LR, RR]] {
      type Delegates = transmittable.Type
    } =
    DelegatingTransmittable(
      provide = (value, context) =>
        if (value == null) null else Right(context delegate compatibility.either.right(value)),
      receive = (value, context) =>
        if (value == null) null else Right(context delegate compatibility.either.right(value)))

  implicit def future[B, I, R]
    (implicit
       transmittable: Transmittable[(Option[B], Option[String]), I, (Option[R], Option[String])])
  : ConnectedTransmittable.Proxy[Future[B], I, Future[R]] {
      type Proxy = Future[R]
      type Internal = Promise[R]
      type Message = transmittable.Type
    } =
    ConnectedTransmittable.Proxy(
      internal = Promise[R](),

      provide = (value, context) => {
        value.value match {
          case Some(Success(value)) =>
            context.endpoint.close()
            Some(value) -> None

          case Some(Failure(value)) =>
            context.endpoint.close()
            None -> Some(RemoteAccessException.serialize(value))

          case _ =>
            value onComplete { completed =>
              val message = completed match {
                case Success(value) => Some(value) -> None
                case Failure(value) => None -> Some(RemoteAccessException.serialize(value))
              }
              context.endpoint.send(message)
              context.endpoint.close()
            }

            None -> None
        }
      },

      receive = (promise, value, context) => {
        def update(value: (Option[R], Option[String])) =
          value match {
            case (Some(value), _) =>
              promise.success(value)
              context.endpoint.close()

            case (_, Some(value)) =>
              promise.failure(RemoteAccessException.deserialize(value))
              context.endpoint.close()

            case _ =>
          }

        update(value)

        context.endpoint.receive foreach update

        context.endpoint.closed foreach { _ =>
          promise.tryFailure(new RemoteAccessException(RemoteAccessException.ChannelClosed))
        }
      },

      direct = (promise, context) => promise.future,

      proxy = (promise, completion, context) => {
        completion foreach { _.failed foreach promise.failure }
        promise.future
      })
}

trait TransmittableCollections
    extends TransmittableIterableCollections
    with TransmittableGeneralCollections {
  this: Transmittable.Base =>

  final implicit def identicalArray[T: IdenticallyTransmittable]
  : IdenticallyTransmittable[Array[T]] = IdenticallyTransmittable()

  final implicit def identicalMap
    [V: IdenticallyTransmittable, K: IdenticallyTransmittable]
  : IdenticallyTransmittable[Map[V, K]] = IdenticallyTransmittable()

  final implicit def identicalOption[T: IdenticallyTransmittable]
  : IdenticallyTransmittable[Option[T]] = IdenticallyTransmittable()

  final implicit def identicalSome[T: IdenticallyTransmittable]
  : IdenticallyTransmittable[Some[T]] = IdenticallyTransmittable()

  final implicit def identicalNone
  : IdenticallyTransmittable[None.type] = IdenticallyTransmittable()

  final implicit def identicalEither
    [L: IdenticallyTransmittable, R: IdenticallyTransmittable]
  : IdenticallyTransmittable[Either[L, R]] = IdenticallyTransmittable()

  final implicit def identicalLeft[L: IdenticallyTransmittable, R]
  : IdenticallyTransmittable[Left[L, R]] = IdenticallyTransmittable()

  final implicit def identicalRight[L, R: IdenticallyTransmittable]
  : IdenticallyTransmittable[Right[L, R]] = IdenticallyTransmittable()
}
