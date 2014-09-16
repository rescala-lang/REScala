package rescala.events

import rescala._
import rescala.propagation._


/**
 * Wrapper for an anonymous function
 */
case class EventHandler[T](fun: T => Unit, dependency: Dependency[T]) extends Dependant {
  override def triggerReevaluation()(implicit turn: Turn): Unit = {}
  override def applyPulse(implicit turn: Turn): Unit = dependency.pulse.valueOption.foreach(fun)
}

/**
 * Base trait for events.
 */
trait EventNode[T] extends Event[T] {
  def +=(react: T => Unit): Unit = EventHandler(react, this).addDependency(this)
  def -=(react: T => Unit): Unit = this.removeDependant(EventHandler(react, this))
}


/**
 * An implementation of an imperative event
 */
class ImperativeEvent[T] extends EventNode[T] {

  /** Trigger the event */
  def apply(v: T): Unit = Turn.newTurn { turn =>
    pulse(ValuePulse(v))(turn)
    turn.startEvaluation()
  }

  override def toString = getClass.getName
}


/** base class for dependent events */
abstract class DependentEvent[T](dependencies: List[Dependency[Any]]) extends EventNode[T] with Dependant {
  dependencies.foreach(addDependency)

  /** this method is called to produce a new change. if it returns None no change is propagated, alse the returned value is propagated. */
  def calculatePulse()(implicit turn: Turn): Pulse[T]

  override def triggerReevaluation()(implicit turn: Turn): Unit = {
    pulse(calculatePulse())
  }

}


/**
 * Used to model the change event of a signal. Keeps the last value
 */
class ChangedEventNode[T](dependency: Dependency[T]) extends DependentEvent[(T, T)](List(dependency)) {
  override def calculatePulse()(implicit turn: Turn): Pulse[(T, T)] = Pulse {
    val pulse = dependency.pulse
    for {old <- pulse.oldOption; value <- pulse.valueOption} yield (old, value)
  }
  override def toString = "(" + " ChangedEventNode" + dependency + ")"
}


/**
 * Implements filtering event by a predicate
 */
class EventNodeFilter[T](ev: Event[T], f: T => Boolean) extends DependentEvent[T](List(ev)) {
  override def calculatePulse()(implicit turn: Turn): Pulse[T] = Pulse(ev.pulse.valueOption.filter(f))
  override def toString = "(" + ev + " && <predicate>)"
}


/**
 * Implements transformation of event parameter
 */
class EventNodeMap[T, U](ev: Event[T], f: T => U) extends DependentEvent[U](List(ev)) {
  override def calculatePulse()(implicit turn: Turn): Pulse[U] = Pulse(ev.pulse.valueOption.map(f))
  override def toString = "(" + ev + " && <predicate>)"
}


/**
 * Implementation of event except
 */
class EventNodeExcept[T, U](accepted: Event[T], except: Event[U]) extends DependentEvent[T](List(accepted, except)) {
  override def calculatePulse()(implicit turn: Turn): Pulse[T] =
    except.pulse match {
      case NoChangePulse => accepted.pulse
      case ValuePulse(value) => NoChangePulse
      case DiffPulse(value, old) => NoChangePulse
    }
  override def toString = "(" + accepted + " \\ " + except + ")"
}


/**
 * Implementation of event disjunction
 */
class EventNodeOr[T](ev1: Event[_ <: T], ev2: Event[_ <: T]) extends DependentEvent[T](List(ev1, ev2)) {
  override def calculatePulse()(implicit turn: Turn): Pulse[T] =
    ev1.pulse match {
      case NoChangePulse => ev2.pulse
      case p@ValuePulse(value) => p
      case p@DiffPulse(value, old) => p
    }
  override def toString = "(" + ev1 + " || " + ev2 + ")"
}


/**
 * Implementation of event conjunction
 */
class EventNodeAnd[T1, T2, T](ev1: Event[T1], ev2: Event[T2], merge: (T1, T2) => T) extends DependentEvent[T](List(ev1, ev2)) {

  override def calculatePulse()(implicit turn: Turn): Pulse[T] = Pulse {
    for {left <- ev1.pulse.valueOption; right <- ev2.pulse.valueOption}
    yield { merge(left, right) }
  }

  override def toString = "(" + ev1 + " and " + ev2 + ")"
}
