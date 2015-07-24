package rescala

import rescala.graph.{PulseOption, Reactive, Pulsing, Spores}
import rescala.turns.Ticket

import scala.collection.LinearSeq
import scala.collection.immutable.Queue

trait Event[+T, S <: Spores] extends PulseOption[T, S]{

  /** add an observer */
  final def +=(react: T => Unit)(implicit ticket: Ticket[S]): Observe[S] = observe(react)(ticket)
  final def observe(react: T => Unit)(implicit ticket: Ticket[S]): Observe[S] = Observe(this)(react)

  /**
   * Events disjunction.
   */
  final def ||[U >: T](other: Event[U, S])(implicit ticket: Ticket[S]): Event[U, S] = Events.or(this, other)

  /**
   * Event filtered with a predicate
   */
  final def &&(pred: T => Boolean)(implicit ticket: Ticket[S]): Event[T, S] = Events.filter(this)(pred)
  final def filter(pred: T => Boolean)(implicit ticket: Ticket[S]): Event[T, S] = &&(pred)

  /**
   * Event is triggered except if the other one is triggered
   */
  final def \[U](other: Event[U, S])(implicit ticket: Ticket[S]): Event[T, S] = Events.except(this, other)

  /**
   * Events conjunction
   */
  final def merge[U, R](other: Event[U, S])(merger: (T, U) => R)(implicit ticket: Ticket[S]): Event[R, S] = Events.merge(this, other)(merger)

  /**
   * Event conjunction with a merge method creating a tuple of both event parameters
   */
  final def zip[U](other: Event[U, S])(implicit ticket: Ticket[S]): Event[(T, U), S] = Events.merge(this, other)((_, _))

  /**
   * Transform the event parameter
   */
  final def map[U](mapping: T => U)(implicit ticket: Ticket[S]): Event[U, S] = Events.map(this)(mapping)

  /**
   * Drop the event parameter; equivalent to map((_: Any) => ())
   */
  final def dropParam(implicit ticket: Ticket[S]): Event[Unit, S] = Events.map(this)(_ => ())


  /** folds events with a given fold function to create a Signal */
  final def fold[A](init: A)(fold: (A, T) => A)(implicit ticket: Ticket[S]): Signal[A, S] = Signals.fold(this, init)(fold)

  /** Iterates a value on the occurrence of the event. */
  final def iterate[A](init: A)(f: A => A)(implicit ticket: Ticket[S]): Signal[A, S] = fold(init)((acc, _) => f(acc))

  /**
   * Counts the occurrences of the event. Starts from 0, when the event has never been
   * fired yet. The argument of the event is simply discarded.
   */
  final def count()(implicit ticket: Ticket[S]): Signal[Int, S] = fold(0)((acc, _) => acc + 1)

  /**
   * Calls f on each occurrence of event e, setting the Signal to the generated value.
   * The initial signal is obtained by f(init)
   */
  final def set[B >: T, A](init: B)(f: (B => A))(implicit ticket: Ticket[S]): Signal[A, S] = fold(f(init))((_, v) => f(v))

  /** returns a signal holding the latest value of the event. */
  final def latest[T1 >: T](init: T1)(implicit ticket: Ticket[S]): Signal[T1, S] = fold(init)((_, v) => v)

  /** Holds the latest value of an event as an Option, None before the first event occured */
  final def latestOption()(implicit ticket: Ticket[S]): Signal[Option[T], S] = fold(None: Option[T]) { (_, v) => Some(v) }

  /** calls factory on each occurrence of event e, resetting the Signal to a newly generated one */
  final def reset[T1 >: T, A](init: T1)(factory: T1 => Signal[A, S])(implicit ticket: Ticket[S]): Signal[A, S] = set(init)(factory).flatten()

  /**
   * Returns a signal which holds the last n events in a list. At the beginning the
   * list increases in size up to when n values are available
   */
  final def last(n: Int)(implicit ticket: Ticket[S]): Signal[LinearSeq[T], S] =
    fold(Queue[T]()) { (queue: Queue[T], v: T) =>
      if (queue.length >= n) queue.tail.enqueue(v) else queue.enqueue(v)
    }

  /** collects events resulting in a variable holding a list of all values. */
  final def list()(implicit ticket: Ticket[S]): Signal[List[T], S] = fold(List[T]())((acc, v) => v :: acc)

  /** Switch back and forth between two signals on occurrence of event e */
  final def toggle[A](a: Signal[A, S], b: Signal[A, S])(implicit ticket: Ticket[S]): Signal[A, S] = ticket { implicit turn =>
    val switched: Signal[Boolean, S] = iterate(false) { !_ }
    Signals.dynamic(switched, a, b) { s => if (switched(s)) b(s) else a(s) }
  }

  /** Return a Signal that is updated only when e fires, and has the value of the signal s */
  final def snapshot[A](s: Signal[A, S])(implicit ticket: Ticket[S]): Signal[A, S] = ticket { turn =>
    Signals.Impl.makeStatic(Set[Reactive[S]](this, s), s.get(turn))((t, current) => this.pulse(t).fold(current, _ => s.get(t)))(turn)
  }

  /** Switch to a new Signal once, on the occurrence of event e. */
  final def switchOnce[A](original: Signal[A, S], newSignal: Signal[A, S])(implicit ticket: Ticket[S]): Signal[A, S] = ticket { implicit turn =>
    val latest = latestOption
    Signals.dynamic(latest, original, newSignal) { t =>
      latest(t) match {
        case None => original(t)
        case Some(_) => newSignal(t)
      }
    }
  }

  /**
   * Switch to a signal once, on the occurrence of event e. Initially the
   * return value is set to the original signal. When the event fires,
   * the result is a constant signal whose value is the value of the event.
   */
  final def switchTo[T1 >: T](original: Signal[T1, S])(implicit ticket: Ticket[S]): Signal[T1, S] = {
    val latest = latestOption
    Signals.dynamic(latest, original) { s =>
      latest(s) match {
        case None => original(s)
        case Some(x) => x
      }
    }
  }

  /** Like latest, but delays the value of the resulting signal by n occurrences */
  final def delay[T1 >: T](init: T1, n: Int)(implicit ticket: Ticket[S]): Signal[T1, S] = {
    val history: Signal[LinearSeq[T], S] = last(n + 1)
    history.map { h => if (h.size <= n) init else h.head }
  }

  /** returns the values produced by the last event produced by mapping this value */
  final def flatMap[B](f: T => Event[B, S])(implicit ticket: Ticket[S]): Event[B, S] = ticket { implicit t =>
    Events.wrapped(map(f).latest(new Evt[B, S]()(t.bufferFactory)))
  }

  /** promotes the latest inner event to an outer event */
  final def flatten[B]()(implicit ticket: Ticket[S], ev: T <:< Event[B, S]): Event[B, S] = flatMap(ev.apply)

  /** logs the events to a signal */
  final def log()(implicit ticket: Ticket[S]): Signal[List[T], S] = fold[List[T]](Nil)((a, v) => v :: a)
}
