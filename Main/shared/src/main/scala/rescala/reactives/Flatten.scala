package rescala.reactives

import rescala.engines.Ticket
import rescala.graph.Struct

import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import scala.reflect.ClassTag

sealed trait Flatten[-A, S <: Struct, R] {
  def apply(sig: Signal[A, S])(implicit ticket: Ticket[S]): R
}
object Flatten {
  implicit def flattenSignal[A, S <: Struct, B](implicit ev: A <:< Signal[B, S]): Flatten[A, S, Signal[B, S]] = new Flatten[A, S, Signal[B, S]] {
    def apply(sig: Signal[A, S])(implicit ticket: Ticket[S]): Signal[B, S] = Signals.dynamic(sig) { s => sig(s)(s) }
  }
  implicit def flattenSignalTraversable
  [A, S <: Struct, B, T[U] <: TraversableLike[U, T[U]], Sig[A1, S1 <: Struct] <: Signal[A1, S1]]
  (implicit ev: A <:< T[Sig[B, S]], cbf: CanBuildFrom[T[_], B, T[B]]): Flatten[A, S, Signal[T[B], S]] = new Flatten[A, S, Signal[T[B], S]] {
    def apply(sig: Signal[A, S])(implicit ticket: Ticket[S]): Signal[T[B], S] = Signals.dynamic(sig) { s => ev(sig(s)) map {_ (s)} }
  }
  implicit def flattenSignalArray
  [A, S <: Struct, B: ClassTag, Sig[U, V <: Struct] <: Signal[U, V]]
  (implicit ev: A <:< Array[Sig[B, S]]): Flatten[A, S, Signal[Array[B], S]] = new Flatten[A, S, Signal[Array[B], S]] {
    def apply(sig: Signal[A, S])(implicit ticket: Ticket[S]): Signal[Array[B], S] = Signals.dynamic(sig) { s => ev(sig(s)) map {_ (s)} }
  }
  implicit def flattenSignalOption
  [A, S <: Struct, B, Sig[U, V <: Struct] <: Signal[U, V]]
  (implicit ev: A <:< Option[Sig[B, S]]): Flatten[A, S, Signal[Option[B], S]] = new Flatten[A, S, Signal[Option[B], S]] {
    def apply(sig: Signal[A, S])(implicit ticket: Ticket[S]): Signal[Option[B], S] = Signals.dynamic(sig) { s => ev(sig(s)) map {_ (s)} }
  }
  implicit def flattenEvent[A, S <: Struct, B](implicit ev: A <:< Event[B, S]): Flatten[A, S, Event[B, S]] = new Flatten[A, S, Event[B, S]] {
    def apply(sig: Signal[A, S])(implicit ticket: Ticket[S]): Event[B, S] = Events.dynamic(sig) { turn => sig(turn)(turn) }

  }
}
