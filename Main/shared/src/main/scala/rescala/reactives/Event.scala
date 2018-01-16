package rescala.reactives

import rescala.core.Pulse.{Exceptional, NoChange, Value}
import rescala.core._
import rescala.macros.MacroAccessors

import scala.collection.immutable.{LinearSeq, Queue}
import scala.language.experimental.macros


/** Occurs based on the dependencies.
  *
  * @tparam T Value type of the event occurrences.
  * @tparam S Internal [[rescala.core.Struct]]ure of state.
  *
  * @groupname operator Event operators
  * @groupprio operator 10
  * @groupname conversion Event to Signal conversions
  * @groupprio conversion 20
  * @groupname accessors Accessors and observers
  * @groupprio accessor 5
  */
trait Event[+T, S <: Struct] extends ReSourciV[Pulse[T], S] with Observable[T, S] with MacroAccessors[Option[T]] with Disconnectable[S] {

  /** Adds an observer.
    * @see observe
    * @group accessor*/
  final def +=(handler: T => Unit)(implicit ticket: CreationTicket[S]): Observe[S] = observe(handler)(ticket)

  /** Uses a partial function `onFailure` to recover an error carried by the event into a value when returning Some(value),
    * or filters the error when returning None */
  final def recover[R >: T](onFailure: PartialFunction[Throwable, Option[R]])(implicit ticket: CreationTicket[S]): Event[R, S] =
    Events.staticNamed(s"(recover $this)", this) { st =>
      st.staticDependPulse(this) match {
        case Exceptional(t) => onFailure.applyOrElse[Throwable, Option[R]](t, throw _).fold[Pulse[R]](Pulse.NoChange)(Pulse.Value(_))
        case other => other
      }
    }

  /** Collects the results from a partial function
    * @group operator */
  final def collect[U](pf: PartialFunction[T, U])(implicit ticket: CreationTicket[S]): Event[U, S] =
    Events.staticNamed(s"(collect $this)", this) { st => st.staticDependPulse(this).collect(pf) }

  /** Events disjunction.
    * @group operator */
  final def ||[U >: T](other: Event[U, S])(implicit ticket: CreationTicket[S]): Event[U, S] = {
    Events.staticNamed(s"(or $this $other)", this, other) { st =>
      val tp = st.staticDependPulse(this)
      if (tp.isChange) tp else st.staticDependPulse(other)
    }
  }

  /** EV filtered with a predicate
    * @group operator*/
  final def filter(pred: T => Boolean)(implicit ticket: CreationTicket[S]): Event[T, S] = Events.staticNamed(s"(filter $this)", this) { st => st.staticDependPulse(this).filter(pred) }
  /** EV filtered with a predicate
    * @group operator*/
  final def &&(pred: T => Boolean)(implicit ticket: CreationTicket[S]): Event[T, S] = filter(pred)

  /** EV is triggered except if the other one is triggered
    * @group operator*/
  final def \[U](except: Event[U, S])(implicit ticket: CreationTicket[S]): Event[T, S] = {
    Events.staticNamed(s"(except $this  $except)", this, except) { st =>
      st.staticDependPulse(except) match {
        case NoChange => st.staticDependPulse(this)
        case Value(_) => Pulse.NoChange
        case ex@Exceptional(_) => ex
      }
    }
  }

  /** Events conjunction
    * @group operator*/
  final def and[U, R](other: Event[U, S])(merger: (T, U) => R)(implicit ticket: CreationTicket[S]): Event[R, S] = {
    Events.staticNamed(s"(and $this $other)", this, other) { st =>
      for {
        left <- st.staticDependPulse(this)
        right <- st.staticDependPulse(other)
      } yield {merger(left, right)}
    }
  }

  /** Event conjunction with a merge method creating a tuple of both event parameters
    * @group operator*/
  final def zip[U](other: Event[U, S])(implicit ticket: CreationTicket[S]): Event[(T, U), S] = and(other)(Tuple2.apply)

  /** Event disjunction with a merge method creating a tuple of both optional event parameters wrapped
    * @group operator*/
  final def zipOuter[U](other: Event[U, S])(implicit ticket: CreationTicket[S]): Event[(Option[T], Option[U]), S] = {
    Events.staticNamed(s"(zipOuter $this $other)", this, other) { st =>
      val left = st.staticDependPulse(this)
      val right = st.staticDependPulse(other)
      if (right.isChange || left.isChange) Value(left.toOption -> right.toOption) else NoChange
    }
  }

  /** Transform the event parameter
    * @group operator*/
  final def map[A](expression: T => A)(implicit ticket: CreationTicket[S]): Event[A, S] = macro rescala.macros.ReactiveMacros.EventMapMacro[T, A, S]
  final def staticMap[U](mapping: T => U)(implicit ticket: CreationTicket[S]): Event[U, S] = Events.staticNamed(s"(map $this)", this) { st => st.staticDependPulse(this).map(mapping) }

  /** Drop the event parameter; equivalent to map((_: Any) => ())
    * @group operator*/
  final def dropParam(implicit ticket: CreationTicket[S]): Event[Unit, S] = staticMap(_ => ())


  /** Folds events with a given operation to create a Signal.
    * @group conversion
    * @usecase def fold[A](init: A)(op: (A, T) => A): rescala.Signal[A]
    * @inheritdoc */
  final def fold[A: ReSerializable](init: A)(op: (A, T) => A)(implicit ticket: CreationTicket[S]): Signal[A, S] = {
    ticket { initialTurn =>
      Signals.staticFold[A, S](Set(this), Pulse.tryCatch(Pulse.Value(init))) { (st, currentValue) =>
        val value = st.staticDependPulse(this).get
        op(currentValue(), value)
      }(initialTurn)(ticket.rename)
    }
  }

  /** reduces events with a given reduce function to create a Signal
    * @group conversion */
  final def reduce[A: ReSerializable](reducer: (=> A, => T) => A)(implicit ticket: CreationTicket[S]): Signal[A, S] =
    ticket { initialTurn =>
      Signals.staticFold[A, S](Set(this), Pulse.empty) { (st, currentValue) =>
        reducer(currentValue(), st.staticDependPulse(this).get)
      }(initialTurn)(ticket.rename)
    }

  /** Applies a function on the current value of the signal every time the event occurs,
    * starting with the init value before the first event occurrence
    * @group conversion */
  final def iterate[A: ReSerializable](init: A)(f: A => A)(implicit ticket: CreationTicket[S]): Signal[A, S] =
    fold(init)((acc, _) => f(acc))

  /** Counts the occurrences of the event. Starts from 0, when the event has never been
    * fired yet. The argument of the event is simply discarded.
    * @group conversion
    * @usecase def count(): rescala.Signal[Int]
    * @inheritdoc */
  final def count()(implicit ticket: CreationTicket[S], ev: ReSerializable[Int]): Signal[Int, S] =
    fold(0)((acc, _) => acc + 1)

  /** Calls f on each occurrence of event e, setting the SL to the generated value.
    * The initial signal is obtained by f(init)
    * @group conversion */
  final def set[B >: T : ReSerializable, A](init: B)(f: (B => A))(implicit ticket: CreationTicket[S]): Signal[A, S] =
    latest(init).map(f)

  /** returns a signal holding the latest value of the event.
    * @group conversion */
  final def latest[T1 >: T : ReSerializable](init: T1)(implicit ticket: CreationTicket[S]): Signal[T1, S] =
    fold(init)((_, v) => v)
  /** returns a signal holding the latest value of the event.
    * @group conversion */
  final def latest[T1 >: T : ReSerializable]()(implicit ticket: CreationTicket[S]): Signal[T1, S] =
    reduce[T1]((_, v) => v)

  /** Holds the latest value of an event as an Option, None before the first event occured
    * @group conversion*/
  final def latestOption[T1 >: T]()(implicit ticket: CreationTicket[S], ev: ReSerializable[Option[T1]]): Signal[Option[T1], S] =
    fold(None: Option[T1]) { (_, v) => Some(v) }

  /** calls factory on each occurrence of event e, resetting the SL to a newly generated one
    * @group conversion*/
  final def reset[T1 >: T : ReSerializable, A, R](init: T1)(factory: T1 => Signal[A, S])
    (implicit ticket: CreationTicket[S], ev: Flatten[Signal[A, S], S, R]): R =
    set(init)(factory).flatten(ev, ticket)

  /** Returns a signal which holds the last n events in a list. At the beginning the
    * list increases in size up to when n values are available
    * @group conversion*/
  final def last[T1 >: T](n: Int)(implicit ticket: CreationTicket[S], ev: ReSerializable[Queue[T1]]): Signal[LinearSeq[T1], S] = {
    fold(Queue[T1]()) { (queue: Queue[T1], v: T) =>
      if (queue.lengthCompare(n) >= 0) queue.tail.enqueue(v) else queue.enqueue(v)
    }
  }

  /** collects events resulting in a variable holding a list of all values.
    * @group conversion*/
  final def list[T1 >: T]()(implicit ticket: CreationTicket[S], ev: ReSerializable[List[T1]]): Signal[List[T1], S] =
    fold(List[T1]())((acc, v) => v :: acc)

  /** Switch back and forth between two signals on occurrence of event e
    * @group conversion*/
  final def toggle[A](a: Signal[A, S], b: Signal[A, S])(implicit ticket: CreationTicket[S], ev: ReSerializable[Boolean]): Signal[A, S] = ticket { ict =>
    val switched: Signal[Boolean, S] = iterate(false) {!_}(ev, ict)
    Signals.dynamic(switched, a, b) { s => if (s.dependDynamic(switched).get) s.dependDynamic(b).get else s.dependDynamic(a).get }(ict)
  }

  /** Switch to a new Signal once, on the occurrence of event e.
    * @group conversion*/
  final def switchOnce[A, T1 >: T](original: Signal[A, S], newSignal: Signal[A, S])(implicit ticket: CreationTicket[S], ev: ReSerializable[Option[T1]]): Signal[A, S] = ticket { turn =>
    val latest = latestOption[T1]()(turn, ev)
    Signals.dynamic(latest, original, newSignal) { t =>
      t.dependDynamic(latest).get match {
        case None => t.dependDynamic(original).get
        case Some(_) => t.dependDynamic(newSignal).get
      }
    }(turn)
  }

  /** Initially the result signal has the value of the original signal.
    * Every time the event fires, the result signal changes to the value of the event,
    * the original signal is no longer used.
    * @group conversion */
  final def switchTo[T1 >: T](original: Signal[T1, S])(implicit ticket: CreationTicket[S], ev: ReSerializable[Option[T1]]): Signal[T1, S] = ticket { turn =>
    val latest = latestOption[T1]()(turn, ev)
    Signals.dynamic(latest, original) { s =>
      s.dependDynamic(latest).get match {
        case None => s.dependDynamic(original).get
        case Some(x) => x
      }
    }(turn)
  }

}
