package loci
package transmitter

import Transmittable.Delegating
import Transmittables.{Delegates, Message}

sealed trait ContextBuilders[S <: Transmittables] {
  import ContextBuilders._

  @inline final def message[B, I, R, P, T <: Transmittables](implicit
    ev: ContextBuilders[S] <:< ContextBuilders[Message[Transmittable.Aux[B, I, R, P, T]]])
  : ContextBuilder[T] =
    ev(this) match { case message: SingleMessage[B, I, R, P, T] =>
      message.contextBuilder
    }

  @inline final def delegate[B, I, R, P, T <: Transmittables](implicit
    ev: ContextBuilders[S] <:< ContextBuilders[Delegates[Transmittable.Aux[B, I, R, P, T]]])
  : ContextBuilder[T] =
    ev(this) match { case delegating: SingleDelegate[B, I, R, P, T] =>
      delegating.contextBuilder
    }

  @inline final def delegatesHead[B, I, R, P, T <: Transmittables, D <: Delegating](implicit
    ev: ContextBuilders[S] <:< ContextBuilders[Delegates[D / Transmittable.Aux[B, I, R, P, T]]])
  : ContextBuilder[T] =
    ev(this) match { case list: List[B, I, R, P, T, D] =>
      list.contextBuilderHead
    }

  @inline final def delegatesTail[B, I, R, P, T <: Transmittables, D <: Delegating](implicit
    ev: ContextBuilders[S] <:< ContextBuilders[Delegates[D / Transmittable.Aux[B, I, R, P, T]]])
  : ContextBuilders[Delegates[D]] =
    ev(this) match { case list: List[B, I, R, P, T, D] =>
      list.contextBuilderTail
    }

  def apply(
    transmittables: S, abstraction: AbstractionRef,
    direction: ContextBuilder.Direction, index: Long)
  : Contexts[S]
}

object ContextBuilders {
  final class SingleMessage[B, I, R, P, T <: Transmittables](
      val contextBuilder: ContextBuilder[T])
    extends ContextBuilders[Message[Transmittable.Aux[B, I, R, P, T]]] {

    def apply(
        transmittables: Message[Transmittable.Aux[B, I, R, P, T]],
        abstraction: AbstractionRef,
        direction: ContextBuilder.Direction,
        index: Long) = {
      val single = contextBuilder(
        transmittables.message.transmittables, abstraction, direction, index)
      new Contexts.SingleMessage(single, single.index)
    }
  }

  final class SingleDelegate[B, I, R, P, T <: Transmittables](
      val contextBuilder: ContextBuilder[T])
    extends ContextBuilders[Delegates[Transmittable.Aux[B, I, R, P, T]]] {

    def apply(
        transmittables: Delegates[Transmittable.Aux[B, I, R, P, T]],
        abstraction: AbstractionRef,
        direction: ContextBuilder.Direction,
        index: Long) = {
      val single = contextBuilder(
        transmittables.delegates.transmittables, abstraction, direction, index)
      new Contexts.SingleDelegate(single, single.index)
    }
  }

  final class List[B, I, R, P, T <: Transmittables, D <: Delegating](
      val contextBuilderHead: ContextBuilder[T],
      val contextBuilderTail: ContextBuilders[Delegates[D]])
    extends ContextBuilders[Delegates[D / Transmittable.Aux[B, I, R, P, T]]] {

    def apply(
        transmittables: Delegates[D / Transmittable.Aux[B, I, R, P, T]],
        abstraction: AbstractionRef,
        direction: ContextBuilder.Direction,
        index: Long) = {
      val tail = contextBuilderTail(
        transmittables.delegates.tailDelegates, abstraction, direction, index)
      val head = contextBuilderHead(
        transmittables.delegates.head.transmittables, abstraction, direction, tail.index)
      new Contexts.List(head, tail, head.index)
    }
  }

  implicit def message[B, I, R, P, T <: Transmittables](implicit
    contextBuilder: ContextBuilder[T])
  : ContextBuilders[Message[Transmittable.Aux[B, I, R, P, T]]] =
    new SingleMessage(contextBuilder)

  implicit def delegate[B, I, R, P, T <: Transmittables](implicit
    contextBuilder: ContextBuilder[T])
  : ContextBuilders[Delegates[Transmittable.Aux[B, I, R, P, T]]] =
    new SingleDelegate(contextBuilder)

  implicit def list[B, I, R, P, T <: Transmittables, D <: Delegating](implicit
    contextBuilder: ContextBuilder[T],
    contextBuilders: ContextBuilders[Delegates[D]])
  : ContextBuilders[Delegates[D / Transmittable.Aux[B, I, R, P, T]]] =
    new List(contextBuilder, contextBuilders)
}
