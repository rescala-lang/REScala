package rescala.events

import rescala.propagation._
import rescala.signals.{Signals, Signal}

import scala.collection.LinearSeq
import scala.collection.immutable.Queue

trait Event[+T] extends Pulsing[T] {

  /** add an event handler */
  def +=(react: T => Unit)(implicit maybe: MaybeTurn): Unit = maybe { turn =>
    turn.register(EventHandler(react, this), Set(this))
  }

  /** remove an event handler */
  def -=(react: T => Unit)(implicit maybe: MaybeTurn): Unit = maybe { turn =>
    turn.unregister(EventHandler(react, this), Set(this))
  }

  /**
   * Events disjunction.
   */
  def ||[U >: T](other: Event[U])(implicit maybe: MaybeTurn): Event[U] = Events.or(this, other)

  /**
   * Event filtered with a predicate
   */
  def &&(pred: T => Boolean)(implicit maybe: MaybeTurn): Event[T] = Events.filter(this)(pred)
  def filter(pred: T => Boolean)(implicit maybe: MaybeTurn): Event[T] = &&(pred)

  /**
   * Event filtered with a boolean variable
   */
  def &&(predicate: => Boolean)(implicit maybe: MaybeTurn): Event[T] = Events.filter[T](this)(_ => predicate)
  def filter(predicate: => Boolean)(implicit maybe: MaybeTurn): Event[T] = &&(predicate)

  /**
   * Event is triggered except if the other one is triggered
   */
  def \[U](other: Event[U])(implicit maybe: MaybeTurn): Event[T] = Events.except(this, other)

  /**
   * Events conjunction
   */
  def and[U, R](other: Event[U], merge: (T, U) => R)(implicit maybe: MaybeTurn): Event[R] = Events.and(this, other, merge)

  /**
   * Event conjunction with a merge method creating a tuple of both event parameters
   */
  def &&[U](other: Event[U])(implicit maybe: MaybeTurn): Event[(T, U)] = Events.and(this, other, (p1: T, p2: U) => (p1, p2))

  /**
   * Transform the event parameter
   */
  def map[U](mapping: T => U)(implicit maybe: MaybeTurn): Event[U] = Events.map(this)(mapping)

  /**
   * Drop the event parameter; equivalent to map((_: Any) => ())
   */
  def dropParam(implicit maybe: MaybeTurn): Event[Unit] = Events.map(this)(_ => ())


  /** folds events with a given fold function to create a Signal */
  def fold[A](init: A)(fold: (A, T) => A)(implicit maybe: MaybeTurn): Signal[A] = Signals.fold(this, init)(fold)

  /** Iterates a value on the occurrence of the event. */
  def iterate[A](init: A)(f: A => A)(implicit maybe: MaybeTurn): Signal[A] = fold(init)((acc, _) => f(acc))

  /**
   * Counts the occurrences of the event. Starts from 0, when the event has never been
   * fired yet. The argument of the event is simply discarded.
   */
  def count()(implicit maybe: MaybeTurn): Signal[Int] = fold(0)((acc, _) => acc + 1)

  /**
   * Calls f on each occurrence of event e, setting the Signal to the generated value.
   *  The initial signal is obtained by f(init)
   */
  def set[B >: T, A](init: B)(f: (B => A))(implicit maybe: MaybeTurn): Signal[A] = fold(f(init))((_, v) => f(v))

  /** returns a signal holding the latest value of the event. */
  def latest[S >: T](init: S)(implicit maybe: MaybeTurn): Signal[S] = fold(init)((_, v) => v)

  /** Holds the latest value of an event as an Option, None before the first event occured */
  def hold()(implicit maybe: MaybeTurn): Signal[Option[T]] = latestOption()
  def latestOption()(implicit maybe: MaybeTurn): Signal[Option[T]] = fold(None: Option[T]){ (_, v) => Some(v) }

  /** calls factory on each occurrence of event e, resetting the Signal to a newly generated one */
  def reset[S >: T, A](init: S)(factory: S => Signal[A])(implicit maybe: MaybeTurn): Signal[A] = set(init)(factory).flatten()

  /**
   * Returns a signal which holds the last n events in a list. At the beginning the
   *  list increases in size up to when n values are available
   */
  def last(n: Int)(implicit maybe: MaybeTurn): Signal[LinearSeq[T]] =
    fold(Queue[T]()) { (queue: Queue[T], v: T) =>
      if (queue.length >= n) queue.tail.enqueue(v) else queue.enqueue(v)
    }

  /** collects events resulting in a variable holding a list of all values. */
  def list()(implicit maybe: MaybeTurn): Signal[List[T]] = fold(List[T]())((acc, v) => v :: acc)

  /** Switch back and forth between two signals on occurrence of event e */
  def toggle[A](a: Signal[A], b: Signal[A]): Signal[A] = {
    val switched: Signal[Boolean] = iterate(false) { !_ }
    Signals.dynamic(switched, a, b) { s => if (switched(s)) b(s) else a(s) }
  }

  /** Return a Signal that is updated only when e fires, and has the value of the signal s */
  def snapshot[A](s: Signal[A])(implicit maybe: MaybeTurn): Signal[A] = fold(s.get)((_, _) => s.get)

  /** Switch to a new Signal once, on the occurrence of event e. */
  def switchOnce[A](original: Signal[A], newSignal: Signal[A])(implicit maybe: MaybeTurn): Signal[A] = {
    val latest = latestOption
    Signals.dynamic(latest, original, newSignal) { s =>
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
  def switchTo[S >: T](original: Signal[S])(implicit maybe: MaybeTurn): Signal[S] = {
    val latest = latestOption
    Signals.dynamic(latest, original) { s =>
      latest(s) match {
        case None => original(s)
        case Some(x) => x
      }
    }
  }

  /** Like latest, but delays the value of the resulting signal by n occurrences */
  def delay[S >: T](init: S, n: Int)(implicit maybe: MaybeTurn): Signal[S] = {
    val history: Signal[LinearSeq[T]] = last(n + 1)
    Signals.dynamic(history) { s =>
      val h = history(s)
      if (h.size <= n) init else h.head
    }
  }
}
