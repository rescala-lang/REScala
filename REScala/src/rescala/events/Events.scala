package rescala.events

import rescala._
import rescala.propagation._
import rescala.signals.Signal

import scala.collection.LinearSeq


trait Event[+T] extends Dependency[T] {

  def +=(react: T => Unit): Unit

  def -=(react: T => Unit): Unit

  /**
   * Events disjunction.
   */
  def ||[S >: T, U <: S](other: Event[U]): Event[S] = new EventNodeOr[S](this, other)

  /**
   * Event filtered with a predicate
   */
  def &&[U >: T](pred: U => Boolean): Event[T] = new EventNodeFilter[T](this, pred)
  def filter[U >: T](pred: U => Boolean) = &&[U](pred)

  /**
   * Event filtered with a boolean variable
   */
  def &&(predicate: => Boolean): Event[T] = new EventNodeFilter[T](this, _ => predicate)
  def filter(predicate: => Boolean): Event[T] = &&(predicate)

  /**
   * Event is triggered except if the other one is triggered
   */
  def \[U](other: Event[U]): Event[T] = new EventNodeExcept(this, other)

  /**
   * Events conjunction
   */
  def and[U, V, S >: T](other: Event[U], merge: (S, U) => V): Event[V] = new EventNodeAnd[S, U, V](this, other, merge)

  /**
   * Event conjunction with a merge method creating a tuple of both event parameters
   */
  def &&[U, S >: T](other: Event[U]): Event[(S, U)] = new EventNodeAnd[S, U, (S, U)](this, other, (p1: S, p2: U) => (p1, p2))

  /**
   * Transform the event parameter
   */
  def map[U, S >: T](mapping: S => U): Event[U] = new EventNodeMap[S, U](this, mapping)

  /**
   * Drop the event parameter; equivalent to map((_: Any) => ())
   */
  def dropParam[S >: T]: Event[Unit] = new EventNodeMap[S, Unit](this, (_: Any) => ())


  def fold[A](init: A)(fold: (A, T) => A): Signal[A] = IFunctions.fold(this, init)(fold)
  def iterate[A](init: A)(f: A => A): Signal[A] = IFunctions.iterate(this, init)(f)
  def count: Signal[Int] = IFunctions.count(this)

  def set[B >: T, A](init: B)(f: (B => A)): Signal[A] = IFunctions.set(this, init)(f)

  def latest[S >: T](init: S): Signal[S] = IFunctions.latest(this, init)
  def hold[S >: T]: Signal[Option[T]] = IFunctions.latestOption[T](this)
  def latestOption[S >: T]: Signal[Option[T]] = IFunctions.latestOption[T](this)

  def reset[S >: T, A](init: S)(f: (S) => Signal[A]): Signal[A] = IFunctions.reset(this, init)(f)

  def last[S >: T](n: Int): Signal[LinearSeq[S]] = IFunctions.last[S](this, n)
  def list[S >: T](): Signal[List[S]] = IFunctions.list[S](this)

  def toggle[A](a: Signal[A], b: Signal[A]): Signal[A] = IFunctions.toggle(this, a, b)
  def snapshot[A](s: Signal[A]): Signal[A] = IFunctions.snapshot(this, s)

  def switchOnce[A](oldS: Signal[A], newS: Signal[A]): Signal[A] = IFunctions.switchOnce(this, oldS, newS)

  def delay[S >: T](init: S, n: Int): Signal[S] = IFunctions.delay(this, init, n)
}


/**
 * Wrapper for an anonymous function
 */
case class EventHandler[T](fun: T => Unit, dependency: Dependency[T]) extends Dependant {
  addDependency(dependency)
  override def dependencyChanged[Q](dep: Dependency[Q])(implicit turn: Turn): Unit = dependency.pulse.valueOption.foreach(fun)
  override def triggerReevaluation()(implicit turn: Turn): Unit = {}
}

/**
 * Base trait for events.
 */
trait EventNode[T] extends Event[T] {
  def +=(react: T => Unit): Unit = EventHandler(react, this)
  def -=(react: T => Unit): Unit = EventHandler(react, this)
}


/**
 * An implementation of an imperative event
 */
class ImperativeEvent[T] extends EventNode[T] {

  /** Trigger the event */
  def apply(v: T): Unit = Turn.newTurn { turn =>
    turn.pulse(this, ValuePulse(v))
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
    turn.pulse(this, calculatePulse())
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

object emptyevent extends Event[Nothing] {
  def +=(react: Nothing => Unit): Unit = { /* do nothing */ }
  def -=(react: Nothing => Unit): Unit = { /* do nothing */ }
}


/**
 * Implementation of an observable method
 */
class Observable[T, U](body: T => U) extends (T => U) {
  // before and after, modeled as primitive events
  lazy val before = new ImperativeEvent[T]
  lazy val after = new ImperativeEvent[(T, U)]

  /**
   * Instrumented method implementation:
   * trigger events before and after the actual method execution
   */
  def apply(t: T): U = {
    before(t)
    val res = body(t)
    after((t, res))
    res
  }
}

object Observable {
  def apply[T, U](f: T => U) = new Observable(f)
}
