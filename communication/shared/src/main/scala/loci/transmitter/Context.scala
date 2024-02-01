package loci
package transmitter

import Transmittable.Delegating
import Transmittables.{Delegates, Message, None}

sealed trait Context[S <: Transmittables] {
  this: ContextBuilder.Context[S] =>

  val remote: RemoteRef

  def endpoint[B, I, R, P, T <: Transmittables](implicit
    ev: Context.Endpoint.Base[S] <:<
        Context.Endpoint.Base[Message[Transmittable.Aux[B, I, R, P, T]]])
  : Endpoint[B, R]
}

object Context {
  trait Providing[S <: Transmittables] extends Context[S] {
    this: ContextBuilder.Context[S] =>

    def provide[B, I, R, P, T <: Transmittables](
      value: B)(implicit selector: Selector.Base[B, I, R, P, T, S]): I
  }

  trait Receiving[S <: Transmittables] extends Context[S] {
    this: ContextBuilder.Context[S] =>

    def receive[B, I, R, P, T <: Transmittables](
      value: I)(implicit selector: Selector.Intermediate[B, I, R, P, T, S]): R
  }

  object Endpoint {
    sealed trait Base[S <: Transmittables] extends Context[S] {
      this: ContextBuilder.Context[S] =>

      @inline final def endpoint[B, I, R, P, T <: Transmittables](implicit
        ev: Base[S] <:< Base[Message[Transmittable.Aux[B, I, R, P, T]]])
      : Endpoint[B, R] =
        ev(this) match { case message: MessageImpl[B, I, R, P, T] =>
          message.endpoint
        }
    }

    trait MessageImpl[B, I, R, P, T <: Transmittables]
      extends Base[Message[Transmittable.Aux[B, I, R, P, T]]] {
      this: ContextBuilder.Context[Message[Transmittable.Aux[B, I, R, P, T]]] =>

      val endpoint: Endpoint[B, R]
    }

    trait DelegatesImpl[D <: Delegating] extends Base[Delegates[D]] {
      this: ContextBuilder.Context[Delegates[D]] =>
    }

    trait NoneImpl extends Base[None] {
      this: ContextBuilder.Context[None] =>
    }
  }
}
