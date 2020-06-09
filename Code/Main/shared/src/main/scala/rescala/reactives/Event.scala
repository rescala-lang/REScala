package rescala.reactives

import rescala.core.Pulse.{Exceptional, NoChange, Value}
import rescala.core.{Interp, _}
import rescala.interface.RescalaInterface
import rescala.macros.cutOutOfUserComputation
import rescala.reactives.Observe.ObserveInteract
import rescala.reactives.RExceptions.ObservedException

import scala.collection.immutable.{LinearSeq, Queue}

/** Events only propagate a value when they are changing,
  * when the system is at rest, events have no values.
  *
  * Note: We hide implicit parameters of the API in the documentation.
  * They are used to ensure correct creation, and you normally do not have to worry about them,
  * except if you accidentally call the implicit parameter list, in which cas you may get cryptic errors.
  * This is a scala limitation.
  * We also hide the internal state parameter of passed and returned rescalaAPI.events.
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

  val rescalaAPI: RescalaInterface[S]
  import rescalaAPI.Signal

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
    *
    * @return the resulting [[Observe]] can be used to remove the observer.
    * @usecase def observe(handler: T => Unit): Observe[S]
    * @group accessor */
  final def observe(onValue: T => Unit,
                    onError: Throwable => Unit = null,
                    fireImmediately: Boolean = false)
                   (implicit ticket: CreationTicket[S]): Observe[S]
  = Observe.strong(this, fireImmediately) { reevalVal =>
    val internalVal = internalAccess(reevalVal)
    new ObserveInteract {
      override def checkExceptionAndRemoval(): Boolean = {
        reevalVal match {
          case Pulse.Exceptional(f) if onError == null =>
            throw new ObservedException(Event.this, "observed", f)
          case _                                       => ()
        }
        false
      }
      override def execute(): Unit = internalVal match {
        case Pulse.NoChange       => ()
        case Pulse.Value(v)       => onValue(v)
        case Pulse.Exceptional(f) => onError(f)
      }
    }
  }

  /** Uses a partial function `onFailure` to recover an error carried by the event into a value when returning Some(value),
    * or filters the error when returning None
    * @usecase def recover[R >: T](onFailure: PartialFunction[Throwable, Option[R]]): rescala.default.Event[R]
    */
  @cutOutOfUserComputation
  final def recover[R >: T](onFailure: PartialFunction[Throwable, Option[R]])(implicit ticket: CreationTicket[S]): Event[R, S] =
    rescalaAPI.Events.staticNamed(s"(recover $this)", this) { st =>
      st.collectStatic(this) match {
        case Exceptional(t) => onFailure.applyOrElse[Throwable, Option[R]](t, throw _).fold[Pulse[R]](Pulse.NoChange)(Pulse.Value(_))
        case other => other
      }
    }

  /** Collects the results from a partial function
    * @usecase def collect[U](pf: PartialFunction[T, U]): rescala.default.Event[U]
    * @group operator */
  @cutOutOfUserComputation
  final def collect[U](expression: PartialFunction[T, U])(implicit ticket: CreationTicket[S]): Event[U, S]
  = macro rescala.macros.ReactiveMacros.ReactiveUsingFunctionMacro[T, U, S, Events.CollectFuncImpl.type, rescalaAPI.Events.type]
    //Events.staticNamed(s"(collect $this)", this) { st => st.collectStatic(this).collect(pf) }

  /** Events disjunction.
    * Propagates the values if any of the events fires.
    * Only propagates the left event if both fire.
    * @usecase def ||[U >: T](other: rescala.default.Event[U]): rescala.default.Event[U]
    * @group operator */
  @cutOutOfUserComputation
  final def ||[U >: T](other: Event[U, S])(implicit ticket: CreationTicket[S]): Event[U, S] = {
    rescalaAPI.Events.staticNamed(s"(or $this $other)", this, other) { st =>
      val tp = st.collectStatic(this)
      if (tp.isChange) tp else other.internalAccess(st.collectStatic(other))
    }
  }

  /** Filters the event, only propagating the value when the filter is true.
    * @usecase def filter(pred: T => Boolean): rescala.default.Event[T]
    * @group operator*/
  @cutOutOfUserComputation
  final def filter(expression: T => Boolean)(implicit ticket: CreationTicket[S]): Event[T, S]
  = macro rescala.macros.ReactiveMacros.ReactiveUsingFunctionMacro[T, T, S, Events.FilterFuncImpl.type, rescalaAPI.Events.type]
  /** Filters the event, only propagating the value when the filter is true.
    * @usecase def &&(pred: T => Boolean): rescala.default.Event[T]
    * @see filter
    * @group operator*/
  @cutOutOfUserComputation
  final def &&(expression: T => Boolean)(implicit ticket: CreationTicket[S]): Event[T, S]
  = macro rescala.macros.ReactiveMacros.ReactiveUsingFunctionMacro[T, T, S, Events.FilterFuncImpl.type, rescalaAPI.Events.type]

  /** Propagates the event only when except does not fire.
    * @usecase def \[U](except: rescala.default.Event[U]): rescala.default.Event[T]
    * @group operator*/
  @cutOutOfUserComputation
  final def \[U](except: Event[U, S])(implicit ticket: CreationTicket[S]): Event[T, S] = {
    rescalaAPI.Events.staticNamed(s"(except $this  $except)", this, except) { st =>
      (except.internalAccess(st.collectStatic(except)): Pulse[U]) match {
        case NoChange => st.collectStatic(this)
        case Value(_) => Pulse.NoChange
        case ex@Exceptional(_) => ex
      }
    }
  }

  /** Merge the event with the other, if both fire simultaneously.
    * @usecase def and[U, R](other: rescala.default.Event[U]): rescala.default.Event[R]
    * @group operator*/
  @cutOutOfUserComputation
  final def and[U, R](other: Event[U, S])(merger: (T, U) => R)(implicit ticket: CreationTicket[S]): Event[R, S] = {
    rescalaAPI.Events.staticNamed(s"(and $this $other)", this, other) { st =>
      for {
        left <- internalAccess(st.collectStatic(this)): Pulse[T]
        right <- other.internalAccess(st.collectStatic(other)) : Pulse[U]
      } yield {merger(left, right)}
    }
  }

  /** Merge the event with the other into a tuple, if both fire simultaneously.
    * @usecase def zip[U](other: rescala.default.Event[U]): rescala.default.Event[(T, U)]
    * @group operator
    * @see and */
  @cutOutOfUserComputation
  final def zip[U](other: Event[U, S])(implicit ticket: CreationTicket[S]): Event[(T, U), S] = and(other)(Tuple2.apply)

  /** Merge the event with the other into a tuple, even if only one of them fired.
    * @usecase def zipOuter[U](other: Event[U, S]): rescala.default.Event[(Option[T], Option[U])]
    * @group operator*/
  @cutOutOfUserComputation
  final def zipOuter[U](other: Event[U, S])(implicit ticket: CreationTicket[S]): Event[(Option[T], Option[U]), S] = {
    rescalaAPI.Events.staticNamed(s"(zipOuter $this $other)", this, other) { st =>
      val left: Pulse[T] = st.collectStatic(this)
      val right: Pulse[U] = other.internalAccess(st.collectStatic(other))
      if (right.isChange || left.isChange) Value(left.toOption -> right.toOption) else NoChange
    }
  }

  /** Transform the event.
    * @usecase def map[A](expression: T => A): rescala.default.Event[A]
    * @group operator*/
  @cutOutOfUserComputation
  final def map[A](expression: T => A)(implicit ticket: CreationTicket[S]): Event[A, S]
  = macro rescala.macros.ReactiveMacros.ReactiveUsingFunctionMacro[T, A, S, Events.MapFuncImpl.type, rescalaAPI.Events.type]

  /** Flattens the inner value.
    * @group operator */
  @cutOutOfUserComputation
  final def flatten[R](implicit flatten: Flatten[Event[T, S], R]): R = flatten.apply(this)


  /** Drop the event parameter; equivalent to map((_: Any) => ())
    * @usecase def dropParam(implicit ticket: CreationTicket[S]): rescala.default.Event[Unit]
    * @group operator*/
  @cutOutOfUserComputation
  final def dropParam(implicit ticket: CreationTicket[S]): Event[Unit, S] = rescalaAPI.Events.static(this)(_ => Some(()))


  /** Folds events with a given operation to create a Signal.
    * @group conversion
    * @usecase def fold[A](init: A)(op: (A, T) => A): rescala.default.Signal[A]
    * @inheritdoc */
  @cutOutOfUserComputation
  final def fold[A](init: A)
                   (op: (A, T) => A)
                   (implicit ticket: CreationTicket[S])
  : Signal[A]
  = macro rescala.macros.ReactiveMacros.EventFoldMacro[T, A, S]

  /** reduces events with a given reduce function to create a Signal
    *
    * @usecase def reduce[A](reducer: (=> A, => T) => A): rescala.default.Signal[A]
    * @group conversion */
  @cutOutOfUserComputation
  final def reduce[A](reducer: (=> A, => T) => A)(implicit ticket: CreationTicket[S]): Signal[A] = {
    val res = ticket.create(
      Set(this),
      Initializer.InitializedSignal[Pulse[A]](Pulse.empty),
      inite = false
    ) { state =>
      new rescalaAPI.Impls.SignalImpl[A](
        initial = state,
        expr = { (st, currentValue) => reducer(currentValue(), st.collectStatic(this).get) },
        name = ticket.rename,
        isDynamicWithStaticDeps = None
        )
    }
    rescalaAPI.Signals.wrapWithSignalAPI(res)
  }

  /** Applies a function on the current value of the signal every time the event occurs,
    * starting with the init value before the first event occurrence
    * @usecase def iterate[A](init: A)(f: A => A): rescala.default.Signal[A]
    * @group conversion */
  @cutOutOfUserComputation
  final def iterate[A](init: A)(f: A => A)(implicit ticket: CreationTicket[S]): Signal[A] =
    rescalaAPI.Events.foldOne(this, init)((acc, _) => f(acc))

  /** Counts the occurrences of the event. Starts from 0, when the event has never been
    * fired yet. The argument of the event is simply discarded.
    * @group conversion
    * @usecase def count(): rescala.default.Signal[Int]
    * @inheritdoc */
  @cutOutOfUserComputation
  final def count()(implicit ticket: CreationTicket[S]): Signal[Int] =
    rescalaAPI.Events.foldOne(this, 0)((acc, _) => acc + 1)

  /** returns a signal holding the latest value of the event.
    * @param init initial value of the returned signal
    * @usecase def latest[A >: T](init: A): rescala.default.Signal[A]
    * @group conversion */
  @cutOutOfUserComputation
  final def latest[A >: T](init: A)(implicit ticket: CreationTicket[S]): Signal[A] =
    rescalaAPI.Events.foldOne(this, init)((_, v) => v)
  /** returns a signal holding the latest value of the event.
    * @usecase def latest[A >: T](): rescala.default.Signal[A]
    * @group conversion */
  @cutOutOfUserComputation
  final def latest[A >: T]()(implicit ticket: CreationTicket[S]): Signal[A] =
    reduce[A]((_, v) => v)

  /** Holds the latest value of an event as an Option, None before the first event occured
    * @usecase def latestOption[A >: T](): rescala.default.Signal[Option[A]]
    * @group conversion*/
  @cutOutOfUserComputation
  final def latestOption[A >: T]()(implicit ticket: CreationTicket[S]): Signal[Option[A]] =
    rescalaAPI.Events.foldOne(this, None: Option[A]) { (_, v) => Some(v) }

  /** Returns a signal which holds the last n events in a list. At the beginning the
    * list increases in size up to when n values are available
    * @usecase def last[A >: T](n: Int): rescala.default.Signal[LinearSeq[A]]
    * @group conversion*/
  @cutOutOfUserComputation
  final def last[A >: T](n: Int)(implicit ticket: CreationTicket[S]): Signal[LinearSeq[A]] = {
    if (n < 0) throw new IllegalArgumentException(s"length must be positive")
    else if (n == 0) rescalaAPI.Var(Nil)
    else rescalaAPI.Events.foldOne(this, Queue[A]()) { (queue: Queue[A], v: T) =>
      if (queue.lengthCompare(n) >= 0) queue.tail.enqueue(v) else queue.enqueue(v)
    }
  }

  /** collects events resulting in a variable holding a list of all values.
    * @usecase def list[A >: T](): rescala.default.Signal[List[A]]
    * @group conversion*/
  @cutOutOfUserComputation
  final def list[A >: T]()(implicit ticket: CreationTicket[S]): Signal[List[A]] =
    rescalaAPI.Events.foldOne(this, List[A]())((acc, v) => v :: acc)

  /** Switch back and forth between two signals on occurrence of event e
    * @usecase def toggle[A](a: rescala.default.Signal[A], b: rescala.default.Signal[A]): rescala.default.Signal[A]
    * @group conversion*/
  @cutOutOfUserComputation
  final def toggle[A](a: Signal[A], b: Signal[A])(implicit ticket: CreationTicket[S])
  : Signal[A] = ticket.transaction { ict =>
    val switched: Signal[Boolean] = iterate(false) {!_}(ict)
    rescalaAPI.Signals.dynamic(switched, a, b) { s => if (s.depend(switched)) s.depend(b) else s.depend(a) }(ict)
  }

}
