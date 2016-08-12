package rescala.reactives

import java.util.concurrent.CompletionException

import rescala.engines.Ticket
import rescala.graph.Pulse.{Change, Exceptional, NoChange, Stable}
import rescala.graph.{Pulse, PulseOption, Reactive, Struct}
import rescala.reactives.RExceptions.{EmptySignalControlThrowable, UnhandledFailureException}

import scala.util.{Failure, Success, Try}

/**
  *
  * Standard implementation of the event interface using Spore-based propagation.
  *
  * @tparam T Type returned when the event fires
  * @tparam S Struct type used for the propagation of the event
  */
trait Event[+T, S <: Struct] extends EventLike[T, S, Signal, Event] with PulseOption[T, S] {

  /** add an observer */
  final def observe(
    onSuccess: T => Unit,
    onFailure: Throwable => Unit = t => throw new UnhandledFailureException(t)
  )(implicit ticket: Ticket[S]): Observe[S] = Observe(this) {
    case Success(v) => onSuccess(v)
    case Failure(t) => onFailure(t)
  }

  def toTry()(implicit ticket: Ticket[S]): Event[Try[T], S] = Events.static(s"(try $this)", this) { turn =>
    Pulse.Change(this.pulse(turn).toOptionTry().getOrElse(throw new IllegalStateException("reevaluation without changes")))
  }


  /**
    * Events disjunction.
    */
  final override def ||[U >: T](other: Event[U, S])(implicit ticket: Ticket[S]): Event[U, S] = {
    Events.static(s"(or $this $other)", this, other) { turn =>
      this.pulse(turn) match {
        case NoChange | Stable(_) => other.pulse(turn)
        case p@Change(_) => p
        case ex@Exceptional(_) => ex
      }
    }
  }


  /**
    * Event filtered with a predicate
    */
  final override def &&(pred: T => Boolean)(implicit ticket: Ticket[S]): Event[T, S] = Events.static(s"(filter $this)", this) { turn => pulse(turn).filter(pred) }

  /** collect results from a partial function */
  final def collect[U](pf: PartialFunction[T, U])(implicit ticket: Ticket[S]): Event[U, S] = Events.static(s"(collect $this)", this) { turn => Pulse.fromOption(get(turn).flatMap(pf.lift))  }

  /**
    * Event is triggered except if the other one is triggered
    */
  final override def \[U](except: Event[U, S])(implicit ticket: Ticket[S]): Event[T, S] = {
    Events.static(s"(except $this  $except)", this, except) { turn =>
      except.pulse(turn) match {
        case NoChange | Stable(_) => this.pulse(turn)
        case Change(_) => Pulse.NoChange
        case ex@Exceptional(_) => ex
      }
    }
  }


  /**
    * Events conjunction
    */
  final override def and[U, R](other: Event[U, S])(merger: (T, U) => R)(implicit ticket: Ticket[S]): Event[R, S] = {
    Events.static(s"(and $this $other)", this, other) { turn =>
      for {
        left <- this.pulse(turn)
        right <- other.pulse(turn)
      } yield {merger(left, right)}
    }
  }

  /**
    * Event conjunction with a merge method creating a tuple of both event parameters
    */
  final override def zip[U](other: Event[U, S])(implicit ticket: Ticket[S]): Event[(T, U), S] = and(other)(Tuple2.apply)

  /**
    * Transform the event parameter
    */
  final override def map[U](mapping: T => U)(implicit ticket: Ticket[S]): Event[U, S] = Events.static(s"(map $this)", this) { turn => pulse(turn).map(mapping) }


  /** folds events with a given fold function to create a Signal */
  final override def fold[A](init: => A)(f: (=> A, T) => A)(implicit ticket: Ticket[S]) = ticket { initialTurn =>
    Signals.Impl.makeStatic(Set[Reactive[S]](this), init) { (turn, currentValue) =>
      get(turn).fold(currentValue)(value => f(currentValue, value))
    }(initialTurn)
  }

  /** reduces events with a given reduce function to create a Signal */
  final def reduce[A](reducer: (=> A, T) => A)(implicit ticket: Ticket[S]) = fold(throw new EmptySignalControlThrowable)(reducer)

  /** Switch back and forth between two signals on occurrence of event e */
  final override def toggle[A](a: Signal[A, S], b: Signal[A, S])(implicit ticket: Ticket[S]): Signal[A, S] = ticket { turn =>
    val switched: Signal[Boolean, S] = iterate(false) {!_}(turn)
    Signals.dynamic(switched, a, b) { s => if (switched(s)) b(s) else a(s) }(turn)
  }

  /** Return a Signal that is updated only when e fires, and has the value of the signal s */
  final override def snapshot[A](s: Signal[A, S])(implicit ticket: Ticket[S]): Signal[A, S] = ticket { turn =>
    Signals.Impl.makeStatic(Set[Reactive[S]](this, s), s.get(turn)) { (t, current) =>
      this.get(t).fold(current)(_ => s.get(t))
    }(turn)
  }

  /** Switch to a new Signal once, on the occurrence of event e. */
  final override def switchOnce[A](original: Signal[A, S], newSignal: Signal[A, S])(implicit ticket: Ticket[S]): Signal[A, S] = ticket { turn =>
    val latest = latestOption()(turn)
    Signals.dynamic(latest, original, newSignal) { t =>
      latest(t) match {
        case None => original(t)
        case Some(_) => newSignal(t)
      }
    }(turn)
  }

  /**
    * Switch to a signal once, on the occurrence of event e. Initially the
    * return value is set to the original signal. When the event fires,
    * the result is a constant signal whose value is the value of the event.
    */
  final override def switchTo[T1 >: T](original: Signal[T1, S])(implicit ticket: Ticket[S]): Signal[T1, S] = ticket { turn =>
    val latest = latestOption()(turn)
    Signals.dynamic(latest, original) { s =>
      latest(s) match {
        case None => original(s)
        case Some(x) => x
      }
    }(turn)
  }

  /** returns the values produced by the last event produced by mapping this value */
  final override def flatMap[B](f: T => Event[B, S])(implicit ticket: Ticket[S]): Event[B, S] = ticket { turn =>
    Events.wrapped(map(f)(turn).latest(Evt()(turn))(turn))(turn)
  }
}

