package rescala.events

import rescala._
import rescala.propagation.Pulse.{Diff, NoChange}
import rescala.propagation._
import rescala.signals.Signal


/**
 * Wrapper for an anonymous function
 */
case class EventHandler[T](fun: T => Unit, dependency: Dependency[T]) extends Event[T] {
  override def reevaluate()(implicit turn: Turn): EvaluationResult = {
    pulse(dependency.pulse)
    EvaluationResult.Done(dependants)
  }
  override def commit(implicit turn: Turn): Unit = {
    pulse.toOption.foreach(fun)
    super.commit
  }
}


/**
 * An implementation of an imperative event
 */
class ImperativeEvent[T] extends Event[T] {

  /** Trigger the event */
  def apply(v: T): Unit = Turn.newTurn { turn =>
    pulse(Pulse.change(v))(turn)
    turn.evaluate(this)
    turn.startEvaluation()
  }

  override protected[rescala] def reevaluate()(implicit turn: Turn): EvaluationResult =
    EvaluationResult.Done(dependants)

  override def toString = getClass.getName
}


object Events {

  def make[T](name: String, dependencies: Dependency[_]*)(calculatePulse: Turn => Pulse[T]): Event[T] = Turn.maybeTurn { turn =>
    val event = new Event[T] {
      override def reevaluate()(implicit turn: Turn): EvaluationResult = {
        pulse(calculatePulse(turn))
        EvaluationResult.Done(dependants)
      }
      override def toString = name
    }
    turn.register(event, dependencies.toSet)
    event
  }

  import scala.language.implicitConversions

  private implicit def optionToPulse[P](option: Option[P]): Pulse[P] = Pulse.fromOption(option)


  /** Used to model the change event of a signal. Keeps the last value */
  def change[T](signal: Signal[T]): Event[(T, T)] =
    make(s"(change $signal)", signal) { turn =>
      signal.pulse(turn) match {
        case Diff(value, Some(old)) => Pulse.change((old, value))
        case NoChange(_) | Diff(_, None) => Pulse.none
      }
    }


  /** Implements filtering event by a predicate */
  def filter[T](ev: Event[T], f: T => Boolean): Event[T] =
    make(s"(filter $ev)", ev) { turn => ev.pulse(turn).toOption.filter(f) }


  /** Implements transformation of event parameter */
  def map[T, U](ev: Event[T], f: T => U): Event[U] =
    make(s"(map $ev)", ev) { turn => ev.pulse(turn).toOption.map(f) }


  /** Implementation of event except */
  def except[T, U](accepted: Event[T], except: Event[U]): Event[T] =
    make(s"(except $accepted  $except)", accepted, except) { turn =>
      except.pulse(turn) match {
        case NoChange(_) => accepted.pulse(turn)
        case Diff(_, _) => Pulse.none
      }
    }


  /** Implementation of event disjunction */
  def or[T](ev1: Event[_ <: T], ev2: Event[_ <: T]): Event[T] =
    make(s"(or $ev1 $ev2)", ev1, ev2) { turn =>
        ev1.pulse(turn) match {
          case NoChange(_) => ev2.pulse(turn)
          case p@Diff(_, _)  => p
        }
    }


  /** Implementation of event conjunction */
  def and[T1, T2, T](ev1: Event[T1], ev2: Event[T2], merge: (T1, T2) => T): Event[T] =
    make(s"(and $ev1 $ev2)", ev1, ev2) { turn => for {
      left <- ev1.pulse(turn).toOption
      right <- ev2.pulse(turn).toOption
    } yield { merge(left, right) }
    }


  /** A wrapped event inside a signal, that gets "flattened" to a plain event node */
  def wrapped[T](wrapper: Signal[Event[T]]): Event[T] = Turn.maybeTurn { creationTurn =>
    new Event[T] {
      override def reevaluate()(implicit turn: Turn): EvaluationResult = ???
    }
  }
}
