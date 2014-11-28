package rescala.events

import rescala.propagation.Pulse.{Diff, NoChange}
import rescala.propagation._
import rescala.propagation.turns.creation.MaybeTurn
import rescala.propagation.turns.{Commitable, Turn, TurnState}
import rescala.signals.Signal


object Events {


  /** Wrapper for an anonymous function to run in the afterCommit phase */
  final case class Observer[T](fun: T => Unit, dependency: Pulsing[T]) extends Reactive with Commitable {
    val cached = TurnState[Option[T]](None, (_, x) => x)

    override protected[rescala] def reevaluate()(implicit turn: Turn): EvaluationResult = {
      cached.set(dependency.pulse.toOption)
      turn.markForCommit(this)
      EvaluationResult.Static(changed = false)
    }
    override def release(implicit turn: Turn): Unit = ()
    override def commit(implicit turn: Turn): Unit = cached.get.foreach(v => turn.afterCommit(fun(v)))
  }


  /** the basic method to create static events */
  def static[T](name: String, dependencies: Reactive*)(calculate: Turn => Pulse[T])(implicit maybe: MaybeTurn): Event[T] = maybe {
    _.create(dependencies.toSet) {
      new Event[T] with StaticReevaluation[T] {
        override def calculatePulse()(implicit turn: Turn): Pulse[T] = calculate(turn)
        override def toString = name
      }
    }
  }

  /** Used to model the change event of a signal. Keeps the last value */
  def change[T](signal: Signal[T])(implicit maybe: MaybeTurn): Event[(T, T)] =
    static(s"(change $signal)", signal) { turn =>
      signal.pulse(turn) match {
        case Diff(value, Some(old)) => Pulse.change((old, value))
        case NoChange(_) | Diff(_, None) => Pulse.none
      }
    }


  /** Implements filtering event by a predicate */
  def filter[T](ev: Pulsing[T])(f: T => Boolean)(implicit maybe: MaybeTurn): Event[T] =
    static(s"(filter $ev)", ev) { turn => ev.pulse(turn).filter(f) }


  /** Implements transformation of event parameter */
  def map[T, U](ev: Pulsing[T])(f: T => U)(implicit maybe: MaybeTurn): Event[U] =
    static(s"(map $ev)", ev) { turn => ev.pulse(turn).map(f) }


  /** Implementation of event except */
  def except[T, U](accepted: Pulsing[T], except: Pulsing[U])(implicit maybe: MaybeTurn): Event[T] =
    static(s"(except $accepted  $except)", accepted, except) { turn =>
      except.pulse(turn) match {
        case NoChange(_) => accepted.pulse(turn)
        case Diff(_, _) => Pulse.none
      }
    }


  /** Implementation of event disjunction */
  def or[T](ev1: Pulsing[_ <: T], ev2: Pulsing[_ <: T])(implicit maybe: MaybeTurn): Event[T] =
    static(s"(or $ev1 $ev2)", ev1, ev2) { turn =>
      ev1.pulse(turn) match {
        case NoChange(_) => ev2.pulse(turn)
        case p@Diff(_, _) => p
      }
    }


  /** Implementation of event conjunction */
  def and[T1, T2, T](ev1: Pulsing[T1], ev2: Pulsing[T2], merge: (T1, T2) => T)(implicit maybe: MaybeTurn): Event[T] =
    static(s"(and $ev1 $ev2)", ev1, ev2) { turn =>
      for {
        left <- ev1.pulse(turn)
        right <- ev2.pulse(turn)
      } yield { merge(left, right) }
    }


  /** A wrapped event inside a signal, that gets "flattened" to a plain event node */
  def wrapped[T](wrapper: Signal[Event[T]])(implicit maybe: MaybeTurn): Event[T] = maybe { creationTurn =>
    creationTurn.create(Set[Reactive](wrapper, wrapper.get(creationTurn))) {
      new Event[T] with DynamicReevaluation[T] {
        override def calculatePulseDependencies(implicit turn: Turn): (Pulse[T], Set[Reactive]) = {
          val inner = wrapper.get
          (inner.pulse, Set(wrapper, inner))
        }
      }
    }
  }
}
