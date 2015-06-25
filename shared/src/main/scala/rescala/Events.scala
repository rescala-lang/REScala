package rescala

import rescala.graph.Pulse.{Diff, NoChange}
import rescala.graph._
import rescala.turns.{Ticket, Turn}


object Events {

  /** the basic method to create static events */
  def static[T, S <: Spores](name: String, dependencies: Reactive[S]*)(calculate: Turn[S] => Pulse[T])(implicit ticket: Ticket[S]): Event[T, S] = ticket { initTurn =>
    val dependencySet: Set[Reactive[S]] = dependencies.toSet
    initTurn.create(dependencySet) {
      new Base(initTurn.bufferFactory, dependencySet) with Event[T, S] with StaticReevaluation[T, S] {
        override def calculatePulse()(implicit turn: Turn[S]): Pulse[T] = calculate(turn)
        override def toString = name
      }
    }
  }

  /** Used to model the change event of a signal. Keeps the last value */
  def change[T, S <: Spores](signal: Signal[T, S])(implicit ticket: Ticket[S]): Event[(T, T), S] =
    static(s"(change $signal)", signal) { turn =>
      signal.pulse(turn) match {
        case Diff(value, Some(old)) => Pulse.change((old, value))
        case NoChange(_) | Diff(_, None) => Pulse.none
      }
    }


  /** Implements filtering event by a predicate */
  def filter[T, S <: Spores](ev: Pulsing[T, S])(f: T => Boolean)(implicit ticket: Ticket[S]): Event[T, S] =
    static(s"(filter $ev)", ev) { turn => ev.pulse(turn).filter(f) }


  /** Implements transformation of event parameter */
  def map[T, U, S <: Spores](ev: Pulsing[T, S])(f: T => U)(implicit ticket: Ticket[S]): Event[U, S] =
    static(s"(map $ev)", ev) { turn => ev.pulse(turn).map(f) }


  /** Implementation of event except */
  def except[T, U, S <: Spores](accepted: Pulsing[T, S], except: Pulsing[U, S])(implicit ticket: Ticket[S]): Event[T, S] =
    static(s"(except $accepted  $except)", accepted, except) { turn =>
      except.pulse(turn) match {
        case NoChange(_) => accepted.pulse(turn)
        case Diff(_, _) => Pulse.none
      }
    }


  /** Implementation of event disjunction */
  def or[T, S <: Spores](ev1: Pulsing[_ <: T, S], ev2: Pulsing[_ <: T, S])(implicit ticket: Ticket[S]): Event[T, S] =
    static(s"(or $ev1 $ev2)", ev1, ev2) { turn =>
      ev1.pulse(turn) match {
        case NoChange(_) => ev2.pulse(turn)
        case p@Diff(_, _) => p
      }
    }


  /** Implementation of event conjunction */
  def and[T1, T2, T, S <: Spores](ev1: Pulsing[T1, S], ev2: Pulsing[T2, S], merge: (T1, T2) => T)(implicit ticket: Ticket[S]): Event[T, S] =
    static(s"(and $ev1 $ev2)", ev1, ev2) { turn =>
      for {
        left <- ev1.pulse(turn)
        right <- ev2.pulse(turn)
      } yield { merge(left, right) }
    }


  /** A wrapped event inside a signal, that gets "flattened" to a plain event node */
  def wrapped[T, S <: Spores](wrapper: Signal[Event[T, S], S])(implicit ticket: Ticket[S]): Event[T, S] = ticket { creationTurn =>
    creationTurn.create(Set[Reactive[S]](wrapper, wrapper.get(creationTurn))) {
      new Base(creationTurn.bufferFactory) with Event[T, S] with DynamicReevaluation[T, S] {
        override def calculatePulseDependencies(implicit turn: Turn[S]): (Pulse[T], Set[Reactive[S]]) = {
          val inner = wrapper.get
          turn.accessDynamic(inner)
          (inner.pulse, Set(wrapper, inner))
        }
      }
    }
  }
}
