package rescala.reactives

import rescala.core.Pulse.{Exceptional, NoChange, Value}
import rescala.core.{Interp, _}
import rescala.macros.cutOutOfUserComputation

import scala.collection.immutable.{LinearSeq, Queue}
import scala.language.experimental.macros
import scala.language.implicitConversions

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
trait Event[+T, S <: Struct] extends ReSource[S] with Interp[Option[T], S] with Disconnectable[S] {


  implicit def internalAccess(v: Value): Pulse[T]

  /** Interprets the pulse of the event by converting to an option
    * @group internal */
  override def interpret(v: Value): Option[T] = v.toOption

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
  )(implicit ticket: CreationTicket[S]): Observe[S] = Observe.strong(this, fireImmediately = false)(v => onSuccess(v.get), onFailure)

  /** Uses a partial function `onFailure` to recover an error carried by the event into a value when returning Some(value),
    * or filters the error when returning None
    * @usecase def recover[R >: T](onFailure: PartialFunction[Throwable, Option[R]]): rescala.Event[R]
    */
  @cutOutOfUserComputation
  final def recover[R >: T](onFailure: PartialFunction[Throwable, Option[R]])(implicit ticket: CreationTicket[S]): Event[R, S] =
    Events.staticNamed(s"(recover $this)", this) { st =>
      st.collectStatic(this) match {
        case Exceptional(t) => onFailure.applyOrElse[Throwable, Option[R]](t, throw _).fold[Pulse[R]](Pulse.NoChange)(Pulse.Value(_))
        case other => other
      }
    }

  /** Collects the results from a partial function
    * @usecase def collect[U](pf: PartialFunction[T, U]): rescala.Event[U]
    * @group operator */
  @cutOutOfUserComputation
  final def collect[U](pf: PartialFunction[T, U])(implicit ticket: CreationTicket[S]): Event[U, S] =
    Events.staticNamed(s"(collect $this)", this) { st => st.collectStatic(this).collect(pf) }

  /** Events disjunction.
    * Propagates the values if any of the events fires.
    * Only propagates the left event if both fire.
    * @usecase def ||[U >: T](other: rescala.Event[U]): rescala.Event[U]
    * @group operator */
  @cutOutOfUserComputation
  final def ||[U >: T](other: Event[U, S])(implicit ticket: CreationTicket[S]): Event[U, S] = {
    Events.staticNamed(s"(or $this $other)", this, other) { st =>
      val tp = st.collectStatic(this)
      if (tp.isChange) tp else other.internalAccess(st.collectStatic(other))
    }
  }

  /** Filters the event, only propagating the value when the filter is true.
    * @usecase def filter(pred: T => Boolean): rescala.Event[T]
    * @group operator*/
  @cutOutOfUserComputation
  final def filter(pred: T => Boolean)(implicit ticket: CreationTicket[S]): Event[T, S] =
    Events.staticNamed(s"(filter $this)", this) { st => st.collectStatic(this).filter(pred) }
  /** Filters the event, only propagating the value when the filter is true.
    * @usecase def &&(pred: T => Boolean): rescala.Event[T]
    * @see filter
    * @group operator*/
  @cutOutOfUserComputation
  final def &&(pred: T => Boolean)(implicit ticket: CreationTicket[S]): Event[T, S] = filter(pred)

  /** Propagates the event only when except does not fire.
    * @usecase def \[U](except: rescala.Event[U]): rescala.Event[T]
    * @group operator*/
  @cutOutOfUserComputation
  final def \[U](except: Event[U, S])(implicit ticket: CreationTicket[S]): Event[T, S] = {
    Events.staticNamed(s"(except $this  $except)", this, except) { st =>
      (except.internalAccess(st.collectStatic(except)): Pulse[U]) match {
        case NoChange => st.collectStatic(this)
        case Value(_) => Pulse.NoChange
        case ex@Exceptional(_) => ex
      }
    }
  }

  /** Merge the event with the other, if both fire simultaneously.
    * @usecase def and[U, R](other: rescala.Event[U]): rescala.Event[R]
    * @group operator*/
  @cutOutOfUserComputation
  final def and[U, R](other: Event[U, S])(merger: (T, U) => R)(implicit ticket: CreationTicket[S]): Event[R, S] = {
    Events.staticNamed(s"(and $this $other)", this, other) { st =>
      for {
        left <- internalAccess(st.collectStatic(this)): Pulse[T]
        right <- other.internalAccess(st.collectStatic(other)) : Pulse[U]
      } yield {merger(left, right)}
    }
  }

  /** Merge the event with the other into a tuple, if both fire simultaneously.
    * @usecase def zip[U](other: rescala.Event[U]): rescala.Event[(T, U)]
    * @group operator
    * @see and */
  @cutOutOfUserComputation
  final def zip[U](other: Event[U, S])(implicit ticket: CreationTicket[S]): Event[(T, U), S] = and(other)(Tuple2.apply)

  /** Merge the event with the other into a tuple, even if only one of them fired.
    * @usecase def zipOuter[U](other: Event[U, S]): rescala.Event[(Option[T], Option[U])]
    * @group operator*/
  @cutOutOfUserComputation
  final def zipOuter[U](other: Event[U, S])(implicit ticket: CreationTicket[S]): Event[(Option[T], Option[U]), S] = {
    Events.staticNamed(s"(zipOuter $this $other)", this, other) { st =>
      val left: Pulse[T] = st.collectStatic(this)
      val right: Pulse[U] = other.internalAccess(st.collectStatic(other))
      if (right.isChange || left.isChange) Value(left.toOption -> right.toOption) else NoChange
    }
  }

  /** Transform the event.
    * @usecase def map[A](expression: T => A): rescala.Event[A]
    * @group operator*/
  @cutOutOfUserComputation
  final def map[A](expression: T => A)(implicit ticket: CreationTicket[S]): Event[A, S] = macro rescala.macros.ReactiveMacros.EventMapMacro[T, A, S]

  /** Flattens the inner value.
    * @group operator */
  @cutOutOfUserComputation
  final def flatten[R](implicit flatten: Flatten[Event[T, S], R]): R = flatten.apply(this)


  /** Drop the event parameter; equivalent to map((_: Any) => ())
    * @usecase def dropParam(implicit ticket: CreationTicket[S]): rescala.Event[Unit]
    * @group operator*/
  @cutOutOfUserComputation
  final def dropParam(implicit ticket: CreationTicket[S]): Event[Unit, S] = Events.static(this)(_ => Some(()))


  /** Folds events with a given operation to create a Signal.
    * @group conversion
    * @usecase def fold[A](init: A)(op: (A, T) => A): rescala.Signal[A]
    * @inheritdoc */
  @cutOutOfUserComputation
  final def fold[A](init: A)(op: (A, T) => A)(implicit ticket: CreationTicket[S], serializable: ReSerializable[A]): Signal[A, S] = macro rescala.macros.ReactiveMacros.EventFoldMacro[T, A, S]

  /** reduces events with a given reduce function to create a Signal
    * @usecase def reduce[A](reducer: (=> A, => T) => A): rescala.Signal[A]
    * @group conversion */
  @cutOutOfUserComputation
  final def reduce[A: ReSerializable](reducer: (=> A, => T) => A)(implicit ticket: CreationTicket[S]): Signal[A, S] =
    ticket { initialTurn =>
      initialTurn.create[Pulse[A], StaticSignal[A, S]](Set(this), Initializer.InitializedSignal[Pulse[A]](Pulse.empty), inite = false) { state =>
        new StaticSignal[A, S](state,
          { (st, currentValue) => reducer(currentValue(), st.collectStatic(this).get) },
          ticket.rename) with DisconnectableImpl[S]
      }
    }

  /** Applies a function on the current value of the signal every time the event occurs,
    * starting with the init value before the first event occurrence
    * @usecase def iterate[A](init: A)(f: A => A): rescala.Signal[A]
    * @group conversion */
  @cutOutOfUserComputation
  final def iterate[A: ReSerializable](init: A)(f: A => A)(implicit ticket: CreationTicket[S]): Signal[A, S] =
    Events.foldOne(this, init)((acc, _) => f(acc))

  /** Counts the occurrences of the event. Starts from 0, when the event has never been
    * fired yet. The argument of the event is simply discarded.
    * @group conversion
    * @usecase def count(): rescala.Signal[Int]
    * @inheritdoc */
  @cutOutOfUserComputation
  final def count()(implicit ticket: CreationTicket[S], ev: ReSerializable[Int]): Signal[Int, S] =
    Events.foldOne(this, 0)((acc, _) => acc + 1)

  /** returns a signal holding the latest value of the event.
    * @param init initial value of the returned signal
    * @usecase def latest[A >: T](init: A): rescala.Signal[A]
    * @group conversion */
  @cutOutOfUserComputation
  final def latest[A >: T : ReSerializable](init: A)(implicit ticket: CreationTicket[S]): Signal[A, S] =
    Events.foldOne(this, init)((_, v) => v)
  /** returns a signal holding the latest value of the event.
    * @usecase def latest[A >: T](): rescala.Signal[A]
    * @group conversion */
  @cutOutOfUserComputation
  final def latest[A >: T : ReSerializable]()(implicit ticket: CreationTicket[S]): Signal[A, S] =
    reduce[A]((_, v) => v)

  /** Holds the latest value of an event as an Option, None before the first event occured
    * @usecase def latestOption[A >: T](): rescala.Signal[Option[A]]
    * @group conversion*/
  @cutOutOfUserComputation
  final def latestOption[A >: T]()(implicit ticket: CreationTicket[S], ev: ReSerializable[Option[A]]): Signal[Option[A], S] =
    Events.foldOne(this, None: Option[A]) { (_, v) => Some(v) }

  /** Returns a signal which holds the last n events in a list. At the beginning the
    * list increases in size up to when n values are available
    * @usecase def last[A >: T](n: Int): rescala.Signal[LinearSeq[A]]
    * @group conversion*/
  @cutOutOfUserComputation
  final def last[A >: T](n: Int)(implicit ticket: CreationTicket[S], ev: ReSerializable[Queue[A]]): Signal[LinearSeq[A], S] = {
    Events.foldOne(this, Queue[A]()) { (queue: Queue[A], v: T) =>
      if (queue.lengthCompare(n) >= 0) queue.tail.enqueue(v) else queue.enqueue(v)
    }
  }

  /** collects events resulting in a variable holding a list of all values.
    * @usecase def list[A >: T](): rescala.Signal[List[A]]
    * @group conversion*/
  @cutOutOfUserComputation
  final def list[A >: T]()(implicit ticket: CreationTicket[S], ev: ReSerializable[List[A]]): Signal[List[A], S] =
    Events.foldOne(this, List[A]())((acc, v) => v :: acc)

  /** Switch back and forth between two signals on occurrence of event e
    * @usecase def toggle[A](a: rescala.Signal[A], b: rescala.Signal[A]): rescala.Signal[A]
    * @group conversion*/
  @cutOutOfUserComputation
  final def toggle[A](a: Signal[A, S], b: Signal[A, S])(implicit ticket: CreationTicket[S], ev: ReSerializable[Boolean]): Signal[A, S] = ticket { ict =>
    val switched: Signal[Boolean, S] = iterate(false) {!_}(ev, ict)
    Signals.dynamic(switched, a, b) { s => if (s.depend(switched)) s.depend(b) else s.depend(a) }(ict)
  }

  /** Switch to a new Signal once, on the occurrence of event e.
    * @usecase def switchOnce[A, B >: T](original: rescala.Signal[A], newSignal: rescala.Signal[A]): rescala.Signal[A]
    * @group conversion*/
  @cutOutOfUserComputation
  final def switchOnce[A, B >: T](original: Signal[A, S], newSignal: Signal[A, S])(implicit ticket: CreationTicket[S], ev: ReSerializable[Option[B]]): Signal[A, S] = ticket { turn =>
    val latest = latestOption[B]()(turn, ev)
    Signals.dynamic(latest, original, newSignal) { t =>
      (t.depend(latest) : Option[B]) match {
        case None => t.depend(original)
        case Some(_) => t.depend(newSignal)
      }
    }(turn)
  }

  /** Initially the result signal has the value of the original signal.
    * Every time the event fires, the result signal changes to the value of the event,
    * the original signal is no longer used.
    * @usecase def switchTo[A >: T](original: rescala.Signal[A]): rescala.Signal[A]
    * @group conversion */
  @cutOutOfUserComputation
  final def switchTo[A >: T](original: Signal[A, S])(implicit ticket: CreationTicket[S], ev: ReSerializable[Option[A]]): Signal[A, S] = ticket { turn =>
    val latest = latestOption[A]()(turn, ev)
    Signals.dynamic(latest, original) { s =>
      s.depend(latest) match {
        case None => s.depend(original)
        case Some(x) => x
      }
    }(turn)
  }


}
