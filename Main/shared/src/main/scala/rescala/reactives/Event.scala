package rescala.reactives

import rescala.core.Pulse.{Exceptional, NoChange, Value}
import rescala.core._
import rescala.macros.MacroAccessors

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.immutable.{LinearSeq, Queue}
import scala.language.experimental.macros


/** Events only propagate a value when they are changing,
  * when the system is at rest, events have no values.
  *
  * Note: We hide implicit parameters of the API in the documentation.
  * They are used to ensure correct creation, and you normally do not have to worry about them,
  * except if you accidentially call the implicit parameterlist, in which cas you may get cryptic errors.
  * This is a scala limitation.
  * We also hide the internal state parameter of passed and returned events.
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
trait Event[+T, S <: Struct] extends ReSourciV[Pulse[T], S] with MacroAccessors[Pulse[T], Option[T], S] with Disconnectable[S] {

  /** Interprets the pulse of the event by converting to an option
    * @group internal */
  override def interpret(v: Pulse[T] @uncheckedVariance): Option[T] = v.toOption

  /** Adds an observer.
    * @usecase def +=(handler: T => Unit): Observe[S]
    * @see observe
    * @group accessor*/
  final def +=(handler: T => Unit)(implicit ticket: CreationTicket[S]): Observe[S] = observe(handler)(ticket)

  /** Add an observer.
    * @return the resulting [[Observe]] can be used to remove the observer.
    * @usecase def observe(handler: T => Unit): Observe[S]
    * @group accessor */
  final def observe(
    onSuccess: T => Unit,
    onFailure: Throwable => Unit = null
  )(implicit ticket: CreationTicket[S]): Observe[S] = Observe.strong(this, fireImmediately = false)(onSuccess, onFailure)

  /** Uses a partial function `onFailure` to recover an error carried by the event into a value when returning Some(value),
    * or filters the error when returning None
    * @usecase recover[R >: T](onFailure: PartialFunction[Throwable, Option[R]]): rescala.Event[R]*/
  final def recover[R >: T](onFailure: PartialFunction[Throwable, Option[R]])(implicit ticket: CreationTicket[S]): Event[R, S] =
    Events.staticNamed(s"(recover $this)", this) { st =>
      st.dependStatic(this) match {
        case Exceptional(t) => onFailure.applyOrElse[Throwable, Option[R]](t, throw _).fold[Pulse[R]](Pulse.NoChange)(Pulse.Value(_))
        case other => other
      }
    }

  /** Collects the results from a partial function
    * @usecase def collect[U](pf: PartialFunction[T, U]): rescala.Event[U]
    * @group operator */
  final def collect[U](pf: PartialFunction[T, U])(implicit ticket: CreationTicket[S]): Event[U, S] =
    Events.staticNamed(s"(collect $this)", this) { st => st.dependStatic(this).collect(pf) }

  /** Events disjunction.
    * @usecase def ||[U >: T](other: Event[U]): rescala.Event[U]
    * @group operator */
  final def ||[U >: T](other: Event[U, S])(implicit ticket: CreationTicket[S]): Event[U, S] = {
    Events.staticNamed(s"(or $this $other)", this, other) { st =>
      val tp = st.dependStatic(this)
      if (tp.isChange) tp else st.dependStatic(other)
    }
  }

  /** Filters the event, only propagating the value when the filter is true.
    * @usecase def filter(pred: T => Boolean): rescala.Event[T]
    * @group operator*/
  final def filter(pred: T => Boolean)(implicit ticket: CreationTicket[S]): Event[T, S] =
    Events.staticNamed(s"(filter $this)", this) { st => st.dependStatic(this).filter(pred) }
  /** Filters the event, only propagating the value when the filter is true.
    * @usecase def fold[A](init: A)(op: (A, T) => A): rescala.Signal[A]
    * @see filter
    * @group operator*/
  final def &&(pred: T => Boolean)(implicit ticket: CreationTicket[S]): Event[T, S] = filter(pred)

  /** Propagates the event only when except does not fire.
    * @usecase def \[U](except: Event[U]): rescala.Event[T]
    * @group operator*/
  final def \[U](except: Event[U, S])(implicit ticket: CreationTicket[S]): Event[T, S] = {
    Events.staticNamed(s"(except $this  $except)", this, except) { st =>
      st.dependStatic(except) match {
        case NoChange => st.dependStatic(this)
        case Value(_) => Pulse.NoChange
        case ex@Exceptional(_) => ex
      }
    }
  }

  /** Merge the event with the other, if both fire simultaneously.
    * @usecase def and[U, R](other: Event[U]): rescala.Event[R]
    * @group operator*/
  final def and[U, R](other: Event[U, S])(merger: (T, U) => R)(implicit ticket: CreationTicket[S]): Event[R, S] = {
    Events.staticNamed(s"(and $this $other)", this, other) { st =>
      for {
        left <- st.dependStatic(this)
        right <- st.dependStatic(other)
      } yield {merger(left, right)}
    }
  }

  /** Merge the event with the other into a tuple, if both fire simultaneously.
    * @usecase def zip[U](other: Event[U]): rescala.Event[(T, U)]
    * @group operator
    * @see and */
  final def zip[U](other: Event[U, S])(implicit ticket: CreationTicket[S]): Event[(T, U), S] = and(other)(Tuple2.apply)

  /** Merge the event with the other into a tuple, even if only one of them fired.
    * @usecase def zipOuter[U](other: Event[U, S]): rescala.Event[(Option[T], Option[U])]
    * @group operator*/
  final def zipOuter[U](other: Event[U, S])(implicit ticket: CreationTicket[S]): Event[(Option[T], Option[U]), S] = {
    Events.staticNamed(s"(zipOuter $this $other)", this, other) { st =>
      val left = st.dependStatic(this)
      val right = st.dependStatic(other)
      if (right.isChange || left.isChange) Value(left.toOption -> right.toOption) else NoChange
    }
  }

  /** Transform the event.
    * @usecase def map[A](expression: T => A): rescala.Event[A]
    * @group operator*/
  final def map[A](expression: T => A)(implicit ticket: CreationTicket[S]): Event[A, S] = macro rescala.macros.ReactiveMacros.EventMapMacro[T, A, S]

  /** Drop the event parameter; equivalent to map((_: Any) => ())
    * @usecase def dropParam(implicit ticket: CreationTicket[S]): rescala.Event[Unit]
    * @group operator*/
  final def dropParam(implicit ticket: CreationTicket[S]): Event[Unit, S] = Events.static(this)(_ => Some(()))


  /** Folds events with a given operation to create a Signal.
    * @group conversion
    * @usecase def fold[A](init: A)(op: (A, T) => A): rescala.Signal[A]
    * @inheritdoc */
  final def fold[A: ReSerializable](init: A)(op: (A, T) => A)(implicit ticket: CreationTicket[S]): Signal[A, S] = {
    ticket { initialTurn =>
      Signals.staticFold[A, S](Set(this), Pulse.tryCatch(Pulse.Value(init))) { (st, currentValue) =>
        val value = st.dependStatic(this).get
        op(currentValue(), value)
      }(initialTurn)(ticket.rename)
    }
  }

  /** reduces events with a given reduce function to create a Signal
    * @usecase def reduce[A](reducer: (=> A, => T) => A): rescala.Signal[A]
    * @group conversion */
  final def reduce[A: ReSerializable](reducer: (=> A, => T) => A)(implicit ticket: CreationTicket[S]): Signal[A, S] =
    ticket { initialTurn =>
      Signals.staticFold[A, S](Set(this), Pulse.empty) { (st, currentValue) =>
        reducer(currentValue(), st.dependStatic(this).get)
      }(initialTurn)(ticket.rename)
    }

  /** Applies a function on the current value of the signal every time the event occurs,
    * starting with the init value before the first event occurrence
    * @usecase def iterate[A](init: A)(f: A => A): rescala.Signal[A]
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
    * @usecase def set[B >: T, A](init: B)(f: (B => A)): rescala.Signal[A]
    * @group conversion */
  final def set[B >: T : ReSerializable, A](init: B)(f: (B => A))(implicit ticket: CreationTicket[S]): Signal[A, S] =
    latest(init).map(f)

  /** returns a signal holding the latest value of the event.
    * @usecase def latest[A >: T](init: A): rescala.Signal[A]
    * @group conversion */
  final def latest[A >: T : ReSerializable](init: A)(implicit ticket: CreationTicket[S]): Signal[A, S] =
    fold(init)((_, v) => v)
  /** returns a signal holding the latest value of the event.
    * @usecase def latest[A >: T](): rescala.Signal[A]
    * @group conversion */
  final def latest[A >: T : ReSerializable]()(implicit ticket: CreationTicket[S]): Signal[A, S] =
    reduce[A]((_, v) => v)

  /** Holds the latest value of an event as an Option, None before the first event occured
    * @usecase def latestOption[A >: T](): rescala.Signal[Option[A]]
    * @group conversion*/
  final def latestOption[A >: T]()(implicit ticket: CreationTicket[S], ev: ReSerializable[Option[A]]): Signal[Option[A], S] =
    fold(None: Option[A]) { (_, v) => Some(v) }

  /** calls factory on each occurrence of event e, resetting the SL to a newly generated one
    * @usecase def reset[T1 >: T, A, R](init: T1)(factory: T1 => Signal[A]): rescala.Signal[A]
    * @group conversion*/
  final def reset[T1 >: T : ReSerializable, A, R](init: T1)(factory: T1 => Signal[A, S])
    (implicit ticket: CreationTicket[S], ev: Flatten[Signal[A, S], S, R]): R =
    set(init)(factory).flatten(ev, ticket)

  /** Returns a signal which holds the last n events in a list. At the beginning the
    * list increases in size up to when n values are available
    * @usecase def last[A >: T](n: Int): rescala.Signal[LinearSeq[A]]
    * @group conversion*/
  final def last[A >: T](n: Int)(implicit ticket: CreationTicket[S], ev: ReSerializable[Queue[A]]): Signal[LinearSeq[A], S] = {
    fold(Queue[A]()) { (queue: Queue[A], v: T) =>
      if (queue.lengthCompare(n) >= 0) queue.tail.enqueue(v) else queue.enqueue(v)
    }
  }

  /** collects events resulting in a variable holding a list of all values.
    * @usecase def list[A >: T](): rescala.Signal[List[A]]
    * @group conversion*/
  final def list[A >: T]()(implicit ticket: CreationTicket[S], ev: ReSerializable[List[A]]): Signal[List[A], S] =
    fold(List[A]())((acc, v) => v :: acc)

  /** Switch back and forth between two signals on occurrence of event e
    * @usecase def toggle[A](a: Signal[A], b: Signal[A]): rescala.Signal[A]
    * @group conversion*/
  final def toggle[A](a: Signal[A, S], b: Signal[A, S])(implicit ticket: CreationTicket[S], ev: ReSerializable[Boolean]): Signal[A, S] = ticket { ict =>
    val switched: Signal[Boolean, S] = iterate(false) {!_}(ev, ict)
    Signals.dynamic(switched, a, b) { s => if (s.dependDynamic(switched).get) s.dependDynamic(b).get else s.dependDynamic(a).get }(ict)
  }

  /** Switch to a new Signal once, on the occurrence of event e.
    * @usecase def switchOnce[A, B >: T](original: Signal[A], newSignal: Signal[A]): rescala.Signal[A]
    * @group conversion*/
  final def switchOnce[A, B >: T](original: Signal[A, S], newSignal: Signal[A, S])(implicit ticket: CreationTicket[S], ev: ReSerializable[Option[B]]): Signal[A, S] = ticket { turn =>
    val latest = latestOption[B]()(turn, ev)
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
    * @usecase def switchTo[A >: T](original: Signal[A]): rescala.Signal[A]
    * @group conversion */
  final def switchTo[A >: T](original: Signal[A, S])(implicit ticket: CreationTicket[S], ev: ReSerializable[Option[A]]): Signal[A, S] = ticket { turn =>
    val latest = latestOption[A]()(turn, ev)
    Signals.dynamic(latest, original) { s =>
      s.dependDynamic(latest).get match {
        case None => s.dependDynamic(original).get
        case Some(x) => x
      }
    }(turn)
  }

}
