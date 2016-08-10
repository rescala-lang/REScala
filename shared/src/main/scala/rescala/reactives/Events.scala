package rescala.reactives

import rescala.engines.Ticket
import rescala.graph.Pulse.{Change, Exceptional, NoChange, Stable}
import rescala.graph._
import rescala.propagation.Turn

import scala.util.{Failure, Success}

object Events {

  private class StaticEvent[T, S <: Struct](_bud: S#SporeP[T, Reactive[S]], expr: Turn[S] => Pulse[T], override val toString: String)
    extends Base[T, S](_bud) with Event[T, S] with StaticReevaluation[T, S] {
    override def calculatePulse()(implicit turn: Turn[S]): Pulse[T] = Pulse.tryCatch(expr(turn))
  }

  private class DynamicEvent[T, S <: Struct](_bud: S#SporeP[T, Reactive[S]], expr: Turn[S] => Pulse[T]) extends Base[T, S](_bud) with Event[T, S] with DynamicReevaluation[T, S] {
    def calculatePulseDependencies(implicit turn: Turn[S]): (Pulse[T], Set[Reactive[S]]) = {
      val (newValueTry, dependencies) = turn.collectDependencies { Globals.reTry(expr(turn)) }
      newValueTry match {
        case Success(p) => (p, dependencies)
        case Failure(t : EmptySignalControlThrowable) => (Pulse.NoChange, dependencies)
        case Failure(t) => (Pulse.Exceptional(t), dependencies)
      }
    }
  }

  /** the basic method to create static events */
  def static[T, S <: Struct](name: String, dependencies: Reactive[S]*)(calculate: Turn[S] => Pulse[T])(implicit ticket: Ticket[S]): Event[T, S] = ticket { initTurn =>
    val dependencySet: Set[Reactive[S]] = dependencies.toSet
    initTurn.create(dependencySet) {
      new StaticEvent[T, S](initTurn.bud(initialIncoming = dependencySet, transient = true), calculate, name)
    }
  }

  /** create dynamic events */
  def dynamic[T, S <: Struct](dependencies: Reactive[S]*)(expr: Turn[S] => Option[T])(implicit ticket: Ticket[S]): Event[T, S] = {
    ticket { initialTurn =>
      initialTurn.create(dependencies.toSet, dynamic = true)(
        new DynamicEvent[T, S](initialTurn.bud(transient = true), expr.andThen(Pulse.fromOption)))
    }
  }


  /** Used to model the change event of a signal. Keeps the last value */
  def change[T, S <: Struct](signal: Signal[T, S])(implicit ticket: Ticket[S]): Event[(T, T), S] =
    static(s"(change $signal)", signal) { turn =>
      signal.pulse(turn) match {
        case Change(value) => signal.stable(turn) match {
          case Stable(oldValue) => Pulse.Change((oldValue, value))
          case ex @ Exceptional(_) => ex
          case _ => throw new IllegalStateException("signal has no value")
        }
        case NoChange | Stable(_) => Pulse.NoChange
        case ex @ Exceptional(t) => ex
      }
    }


  /** Implements filtering event by a predicate */
  def filter[T, S <: Struct](ev: Pulsing[T, S])(f: T => Boolean)(implicit ticket: Ticket[S]): Event[T, S] =
    static(s"(filter $ev)", ev) { turn => ev.pulse(turn).filter(f) }


  /** Implements transformation of event parameter */
  def map[T, U, S <: Struct](ev: Pulsing[T, S])(f: T => U)(implicit ticket: Ticket[S]): Event[U, S] =
    static(s"(map $ev)", ev) { turn => ev.pulse(turn).map(f) }


  /** Implementation of event except */
  def except[T, U, S <: Struct](accepted: Pulsing[T, S], except: Pulsing[U, S])(implicit ticket: Ticket[S]): Event[T, S] =
    static(s"(except $accepted  $except)", accepted, except) { turn =>
      except.pulse(turn) match {
        case NoChange | Stable(_) => accepted.pulse(turn)
        case Change(_) => Pulse.NoChange
        case ex @ Exceptional(_) => ex
      }
    }


  /** Implementation of event disjunction */
  def or[T, S <: Struct](ev1: Pulsing[_ <: T, S], ev2: Pulsing[_ <: T, S])(implicit ticket: Ticket[S]): Event[T, S] =
    static(s"(or $ev1 $ev2)", ev1, ev2) { turn =>
      ev1.pulse(turn) match {
        case NoChange | Stable(_) => ev2.pulse(turn)
        case p@Change(_) => p
        case ex @ Exceptional(_) => ex
      }
    }


  /** Implementation of event conjunction */
  def and[T1, T2, T, S <: Struct](ev1: Pulsing[T1, S], ev2: Pulsing[T2, S])(merge: (T1, T2) => T)(implicit ticket: Ticket[S]): Event[T, S] =
    static(s"(and $ev1 $ev2)", ev1, ev2) { turn =>
      for {
        left <- ev1.pulse(turn)
        right <- ev2.pulse(turn)
      } yield {merge(left, right)}
    }


  /** A wrapped event inside a signal, that gets "flattened" to a plain event node */
  def wrapped[T, S <: Struct](wrapper: Signal[Event[T, S], S])(implicit ticket: Ticket[S]): Event[T, S] = ticket { creationTurn =>
    creationTurn.create(Set[Reactive[S]](wrapper, wrapper.get(creationTurn))) {
      new Base[T, S](creationTurn.bud(transient = true)) with Event[T, S] with DynamicReevaluation[T, S] {
        override def calculatePulseDependencies(implicit turn: Turn[S]): (Pulse[T], Set[Reactive[S]]) = {
          val inner = wrapper.get
          turn.dependencyInteraction(inner)
          (inner.pulse, Set(wrapper, inner))
        }
      }
    }
  }
}
