package loci
package transmitter

import Transmittable.Delegating
import Transmittables.{Delegates, Message, None}

import java.util.concurrent.atomic.AtomicLong

import scala.annotation.implicitNotFound
import scala.util.{Failure, Success}

@implicitNotFound("Message for some transmittable not serializable: ${T}")
trait ContextBuilder[T <: Transmittables] {
  def apply(
     transmittables: T,  abstraction: AbstractionRef,
     direction: ContextBuilder.Direction, index: Long = 0L)
  : ContextBuilder.Context[T]
}

object ContextBuilder {
  sealed class Direction private[ContextBuilder]()

  val sending = new Direction
  val receiving = new Direction

  sealed abstract class Context[S <: Transmittables](
      val remote: RemoteRef,
      val transmittables: S,
      val index: Long,
      val contexts: Contexts[S])
    extends Context.Providing[S] with Context.Receiving[S] {

    def provide[B, I, R, P, T <: Transmittables](
        value: B)(implicit selector: Selector.Base[B, I, R, P, T, S]) = {
      implicit val context = selector.context(contexts)
      selector.transmittable(transmittables).buildIntermediate(value)
    }

    def receive[B, I, R, P, T <: Transmittables](
        value: I)(implicit selector: Selector.Intermediate[B, I, R, P, T, S]) = {
      implicit val context = selector.context(contexts)
      selector.transmittable(transmittables).buildResult(value)
    }
  }

  implicit def messaging[B, I, R, P, T <: Transmittables](implicit
      contextBuilder: ContextBuilder[T],
      serializer: Serializable[I])
  : ContextBuilder[Message[Transmittable.Aux[B, I, R, P, T]]] = {
    type M = Message[Transmittable.Aux[B, I, R, P, T]]

    new ContextBuilder[M] {
      def apply(
          transmittables: M, abstraction: AbstractionRef,
          direction: Direction, index: Long) = {
        val messagingAbstraction = abstraction.derive(index.toString)
        val transmittable = transmittables.message
        val context = contextBuilder(
          transmittable.transmittables,
          messagingAbstraction.derive("~0"),
          direction)

        new Context[M](
            abstraction.remote, transmittables, index + 1L,
            new Contexts.SingleMessage(context, index))
          with Context.Endpoint.MessageImpl[B, I, R, P, T] {

          val sendingTurn = new AtomicLong(1)
          val receivingTurn = new AtomicLong(1)

          def serialize(value: B) = {
            val turn = sendingTurn.getAndIncrement()
            val directedTurn = if (direction == sending) s"+$turn" else s"-$turn"
            implicit val context = contextBuilder(
              transmittable.transmittables,
              messagingAbstraction.derive(directedTurn),
              direction)
            serializer.serialize(transmittable.buildIntermediate(value))
          }

          def deserialize(value: MessageBuffer) = {
            val turn = receivingTurn.getAndIncrement()
            val directedTurn = if (direction == sending) s"-$turn" else s"+$turn"
            implicit val context = contextBuilder(
              transmittable.transmittables,
              messagingAbstraction.derive(directedTurn),
              direction)
            serializer.deserialize(value) map transmittable.buildResult
          }

          val endpoint = new Endpoint[B, R] {
            val doReceive = Notice.Stream[R]

            val closed = messagingAbstraction.channel.closed

            def close() = messagingAbstraction.channel.close()

            def send(value: B) =
              messagingAbstraction.channel.send(serialize(value))

            val receive = doReceive.notice

            messagingAbstraction.channel.receive foreach { value =>
              deserialize(value) match {
                case Failure(exception) =>
                  logging.warn(s"unprocessed channel message: $value", exception)
                case Success(value) =>
                  doReceive.fire(value)
              }
            }
          }
        }
      }
    }
  }

  implicit def delegating[D <: Delegating](implicit
      contextBuilders: ContextBuilders[Delegates[D]])
  : ContextBuilder[Delegates[D]] =
    new ContextBuilder[Delegates[D]] {
      def apply(
          transmittables: Delegates[D], abstraction: AbstractionRef,
          direction: Direction, index: Long) = {
        val context = contextBuilders(transmittables, abstraction, direction, index)
        new Context[Delegates[D]](abstraction.remote, transmittables, context.index, context)
          with Context.Endpoint.DelegatesImpl[D]
      }
    }

  implicit def none: ContextBuilder[None] =
    new ContextBuilder[None] {
      def apply(
          transmittables: None, abstraction: AbstractionRef,
          direction: Direction, index: Long) =
        new Context[None](abstraction.remote, transmittables, index, Contexts.None)
          with Context.Endpoint.NoneImpl
    }
}
