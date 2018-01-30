package rescala.reactives

import rescala.core.{CreationTicket, Struct}

import scala.annotation.implicitNotFound
import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import scala.reflect.ClassTag

@implicitNotFound(msg = "Could not flatten ${A}. Try to select a specific flatten strategy from rescala.reactives.Flatten.")
sealed trait Flatten[-A, S <: Struct, R] {
  def apply(sig: Signal[A, S])(implicit ticket: CreationTicket[S]): R
}
object Flatten {
  implicit def flattenSignal[S <: Struct, B]: Flatten[Signal[B, S], S, Signal[B, S]] = new Flatten[Signal[B, S], S, Signal[B, S]] {
    def apply(sig: Signal[Signal[B, S], S])(implicit ticket: CreationTicket[S]): Signal[B, S] = { Signals.dynamic(sig) { s => s.depend(s.depend(sig)) }}
  }
  implicit def flattenSignalTraversableSignal
  [S <: Struct, B, T[U] <: TraversableLike[U, T[U]], Sig[A1, S1 <: Struct] <: Signal[A1, S1]]
  (implicit cbf: CanBuildFrom[T[_], B, T[B]]): Flatten[T[Sig[B, S]], S, Signal[T[B], S]] = new Flatten[T[Sig[B, S]], S, Signal[T[B], S]] {
    def apply(sig: Signal[T[Sig[B, S]], S])(implicit ticket: CreationTicket[S]): Signal[T[B], S] = Signals.dynamic(sig) { s => s.depend(sig) map { (r: Signal[B, S]) => s.depend(r)} }
  }
  implicit def flattenSignalTraversableEvent
  [S <: Struct, B, T[U] <: TraversableLike[U, T[U]], Evnt[A1, S1 <: Struct] <: Event[A1, S1]]
  (implicit cbf: CanBuildFrom[T[_], Option[B], T[Option[B]]]): Flatten[T[Evnt[B, S]], S, Event[T[Option[B]], S]] = new Flatten[T[Evnt[B, S]], S, Event[T[Option[B]], S]] {
    def apply(sig: Signal[T[Evnt[B, S]], S])(implicit ticket: CreationTicket[S]): Event[T[Option[B]], S] = Events.dynamic(sig) { s =>
      val all = s.depend(sig) map { (r: Event[B, S]) => s.depend(r)}
      if(all.exists(_.isDefined)) Some(all) else None
    }
  }
  implicit def flattenSignalArray
  [S <: Struct, B: ClassTag, Sig[U, V <: Struct] <: Signal[U, V]]
  : Flatten[Array[Sig[B, S]], S, Signal[Array[B], S]] = new Flatten[Array[Sig[B, S]], S, Signal[Array[B], S]] {
    def apply(sig: Signal[Array[Sig[B, S]], S])(implicit ticket: CreationTicket[S]): Signal[Array[B], S] = Signals.dynamic(sig) { s => s.depend(sig) map { (r: Signal[B, S]) => s.depend(r)} }
  }
  implicit def flattenSignalOption
  [S <: Struct, B, Sig[U, V <: Struct] <: Signal[U, V]]
  : Flatten[Option[Sig[B, S]], S, Signal[Option[B], S]] = new Flatten[Option[Sig[B, S]], S, Signal[Option[B], S]] {
    def apply(sig: Signal[Option[Sig[B, S]], S])(implicit ticket: CreationTicket[S]): Signal[Option[B], S] = Signals.dynamic(sig) { s => s.depend(sig) map { (r: Signal[B, S]) => s.depend(r)} }
  }
  implicit def flattenEvent[A, S <: Struct, B]: Flatten[Event[B, S], S, Event[B, S]] = new Flatten[Event[B, S], S, Event[B, S]] {
    def apply(sig: Signal[Event[B, S], S])(implicit ticket: CreationTicket[S]): Event[B, S] = Events.dynamic(sig) { s => s.depend(s.depend(sig)) }

  }
}
