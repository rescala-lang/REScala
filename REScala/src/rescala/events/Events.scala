package rescala.events

import rescala._
import rescala.propagation._
import rescala.signals.Signal


/**
 * Wrapper for an anonymous function
 */
case class EventHandler[T](fun: T => Unit, dependency: Dependency[T]) extends Event[T] with Dependant {
  override def reevaluate()(implicit turn: Turn): EvaluationResult = {
    pulse(dependency.pulse)
    EvaluationResult.Done(dependants)
  }
  override def commit(implicit turn: Turn): Unit = {
    pulse.valueOption.foreach(fun)
    super.commit
  }
}


/**
 * An implementation of an imperative event
 */
class ImperativeEvent[T] extends Event[T] {

  /** Trigger the event */
  def apply(v: T): Unit = Turn.newTurn { turn =>
    pulse(ValuePulse(v))(turn)
    turn.evaluate(this)
    turn.startEvaluation()
  }

  override protected[rescala] def reevaluate()(implicit turn: Turn): EvaluationResult =
    EvaluationResult.Done(dependants)

  override def toString = getClass.getName
}


/** base class for dependent events */
abstract class DependentEvent[T](dependencies: Set[Dependency[Any]])(creationTurn: Turn)
  extends Event[T] with Dependant {

  staticDependencies(dependencies)(creationTurn)

  /** this method is called to produce a new pulse */
  def calculatePulse()(implicit turn: Turn): Pulse[T]

  override def reevaluate()(implicit turn: Turn): EvaluationResult = {
    pulse(calculatePulse())
    EvaluationResult.Done(dependants)
  }

}

object Events {
  /** Used to model the change event of a signal. Keeps the last value */
  def changed[T](dependency: Dependency[T]): Event[(T, T)] = Turn.maybeTurn { turn =>
    new DependentEvent[(T, T)](Set(dependency))(turn) {
      override def calculatePulse()(implicit turn: Turn): Pulse[(T, T)] = Pulse {
        val pulse = dependency.pulse
        for {old <- pulse.oldOption; value <- pulse.valueOption} yield (old, value)
      }
      override def toString = "(" + " ChangedEventNode" + dependency + ")"
    }
  }


  /** Implements filtering event by a predicate */
  def filter[T](ev: Event[T], f: T => Boolean): Event[T] = Turn.maybeTurn { turn =>
    new DependentEvent[T](Set(ev))(turn) {
      override def calculatePulse()(implicit turn: Turn): Pulse[T] = Pulse(ev.pulse.valueOption.filter(f))
      override def toString = "(" + ev + " && <predicate>)"
    }
  }


  /** Implements transformation of event parameter */
  def map[T, U](ev: Event[T], f: T => U): Event[U] = Turn.maybeTurn { turn =>
    new DependentEvent[U](Set(ev))(turn) {
      override def calculatePulse()(implicit turn: Turn): Pulse[U] = Pulse(ev.pulse.valueOption.map(f))
      override def toString = "(" + ev + " && <predicate>)"
    }
  }


  /** Implementation of event except */
  def except[T, U](accepted: Event[T], except: Event[U]): Event[T] = Turn.maybeTurn { turn =>
    new DependentEvent[T](Set(accepted, except))(turn) {
      override def calculatePulse()(implicit turn: Turn): Pulse[T] =
        except.pulse match {
          case NoChangePulse => accepted.pulse
          case ValuePulse(value) => NoChangePulse
          case DiffPulse(value, old) => NoChangePulse
        }
      override def toString = "(" + accepted + " \\ " + except + ")"
    }
  }


  /** Implementation of event disjunction */
  def or[T](ev1: Event[_ <: T], ev2: Event[_ <: T]): Event[T] = Turn.maybeTurn { turn =>
    new DependentEvent[T](Set(ev1, ev2))(turn) {
      override def calculatePulse()(implicit turn: Turn): Pulse[T] =
        ev1.pulse match {
          case NoChangePulse => ev2.pulse
          case p@ValuePulse(value) => p
          case p@DiffPulse(value, old) => p
        }
      override def toString = "(" + ev1 + " || " + ev2 + ")"
    }
  }


  /** Implementation of event conjunction */
  def and[T1, T2, T](ev1: Event[T1], ev2: Event[T2], merge: (T1, T2) => T): Event[T] = Turn.maybeTurn { turn =>
    new DependentEvent[T](Set(ev1, ev2))(turn) {
      override def calculatePulse()(implicit turn: Turn): Pulse[T] = Pulse {
        for {left <- ev1.pulse.valueOption; right <- ev2.pulse.valueOption}
        yield { merge(left, right) }
      }
      override def toString = "(" + ev1 + " and " + ev2 + ")"
    }
  }


  /** A wrapped event inside a signal, that gets "flattened" to a plain event node */
  def wrapped[T](wrapper: Signal[Event[T]]): Event[T] = Turn.maybeTurn { creationTurn =>
    new Event[T] with Dependant {

      setDependencies(Set(wrapper, wrapper.pulse(creationTurn).valueOption.get))(creationTurn)

      override def reevaluate()(implicit turn: Turn): EvaluationResult =
        wrapper.pulse match {
          case NoChangePulse =>
            throw new IllegalStateException("signals are assumed to always pulse")
          case ValuePulse(value) =>
            pulse(value.pulse)
            EvaluationResult.Done(dependants)
          case DiffPulse(value, old) if value != old =>
            val oldLevel = level
            removeDependency(old)
            addDependency(value)
            if (value.level > oldLevel) EvaluationResult.Retry(Set(wrapper, value))
            else {
              pulse(value.pulse)
              EvaluationResult.Done(dependants)
            }
          case DiffPulse(value, old) if value == old =>
            pulse(value.pulse)
            EvaluationResult.Done(dependants)
        }
    }
   }
}
