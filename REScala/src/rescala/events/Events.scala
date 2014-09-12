package rescala.events

import rescala._
import rescala.propagation._
import rescala.signals.{SignalSynt, FoldedSignal, Signal}

import scala.collection.LinearSeq
import scala.collection.immutable.Queue


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


  /** folds events with a given fold function to create a Signal */
  def fold[A](init: A)(fold: (A, T) => A): Signal[A] = Turn.maybeTurn { turn => new FoldedSignal(this, init, fold)(turn) }

  /** Iterates a value on the occurrence of the event. */
  def iterate[A](init: A)(f: A => A): Signal[A] = fold(init)((acc, _) => f(acc))

  /**
   * Counts the occurrences of the event. Starts from 0, when the event has never been
   * fired yet. The argument of the event is simply discarded.
   */
  def count: Signal[Int] = fold(0)((acc, _) => acc + 1)

  /**
   * Calls f on each occurrence of event e, setting the Signal to the generated value.
   *  The initial signal is obtained by f(init)
   */
  def set[B >: T, A](init: B)(f: (B => A)): Signal[A] = fold(f(init))((_, v) => f(v))

  /** returns a signal holding the latest value of the event. */
  def latest[S >: T](init: S): Signal[S] = fold(init)((_, v) => v)

  /** Holds the latest value of an event as an Option, None before the first event occured */
  def hold[S >: T]: Signal[Option[T]] = latestOption
  def latestOption[S >: T]: Signal[Option[T]] = fold(None: Option[T]){ (_, v) => Some(v) }

  /** calls factory on each occurrence of event e, resetting the Signal to a newly generated one */
  def reset[S >: T, A](init: S)(factory: S => Signal[A]): Signal[A] =  {
    val ref: Signal[Signal[A]] = set(init)(factory)
    SignalSynt { s: SignalSynt[A] => ref(s)(s) } // cannot express without high order signals
  }

  /**
   * Returns a signal which holds the last n events in a list. At the beginning the
   *  list increases in size up to when n values are available
   */
  def last[S >: T](n: Int): Signal[LinearSeq[S]] =
    fold(Queue[T]()) { (acc: Queue[T], v: T) =>
      if (acc.length >= n) acc.tail.enqueue(v) else acc.enqueue(v)
    }

  /** collects events resulting in a variable holding a list of all values. */
  def list[S >: T](): Signal[List[S]] = fold(List[T]())((acc, v) => v :: acc)

  /** Switch back and forth between two signals on occurrence of event e */
  def toggle[A](a: Signal[A], b: Signal[A]): Signal[A] = {
    val switched: Signal[Boolean] = iterate(false) { !_ }
    SignalSynt[A](switched, a, b) { s => if (switched(s)) b(s) else a(s) }
  }

  /** Return a Signal that is updated only when e fires, and has the value of the signal s */
  def snapshot[A](s: Signal[A]): Signal[A] = fold(s.get)((_, _) => s.get)

  /** Switch to a new Signal once, on the occurrence of event e. */
  def switchOnce[A](original: Signal[A], newSignal: Signal[A]): Signal[A] = {
    val latest = latestOption
    SignalSynt[A](latest, original, newSignal) { s =>
      latest(s) match {
        case None => original(s)
        case Some(_) => newSignal(s)
      }
    }
  }

  /**
   * Switch to a signal once, on the occurrence of event e. Initially the
   *  return value is set to the original signal. When the event fires,
   *  the result is a constant signal whose value is the value of the event.
   */
  def switchTo[S >: T](original: Signal[S]): Signal[S] = {
    val latest = latestOption
    SignalSynt[S](latest, original) { s =>
      latest(s) match {
        case None => original(s)
        case Some(x) => x
      }
    }
  }

  /** Like latest, but delays the value of the resulting signal by n occurrences */
  def delay[S >: T](init: S, n: Int): Signal[S] = {
    val history: Signal[LinearSeq[T]] = last(n + 1)
    SignalSynt[S](history) { s =>
      val h = history(s)
      if (h.size <= n) init else h.head
    }
  }
}


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
