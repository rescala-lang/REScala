package rescala.operator

import rescala.core.*
import rescala.macros.MacroAccess
import rescala.structure.Pulse.{Exceptional, NoChange, Value}
import rescala.structure.RExceptions.{EmptySignalControlThrowable, ObservedException}
import rescala.structure.{ChangeEventImpl, Diff, EventImpl, Observe, Pulse}

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.immutable.{LinearSeq, Queue}

trait EventBundle extends FoldBundle {
  bundle: Operators =>

  /** Events only propagate a value when they are changing,
    * when the system is at rest, events have no values.
    *
    * Note: We hide implicit parameters of the API in the documentation.
    * They are used to ensure correct creation, and you normally do not have to worry about them,
    * except if you accidentally call the implicit parameter list, in which cas you may get cryptic errors.
    * This is a scala limitation.
    * We also hide the internal state parameter of passed and returned events.
    *
    * @tparam T Value type of the event occurrences.
    *
    * @groupname operator Event operators
    * @groupprio operator 10
    * @groupname conversion Event to Signal conversions
    * @groupprio conversion 20
    * @groupname accessors Accessors and observers
    * @groupprio accessor 5
    */
  trait Event[+T] extends MacroAccess[Option[T]] with Disconnectable {

    type State[V] = bundle.BundleState[V]

    implicit def internalAccess(v: Value): Pulse[T]

    /** Interprets the pulse of the event by converting to an option
      *
      * @group internal
      */
    override def read(v: Value): Option[T] = v.toOption

    /** Add an observer.
      *
      * @return the resulting [[rescala.operator.ObserveBundle.Observe]] can be used to remove the observer.
      * @group accessor
      */
    final def observe(onValue: T => Unit, onError: Throwable => Unit = null, fireImmediately: Boolean = false)(implicit
        ticket: CreationTicket[State]
    ): Disconnectable =
      Observe.strong(this, fireImmediately) { reevalVal =>
        val internalVal: Pulse[T] = (reevalVal)
        new Observe.ObserveInteract {
          override def checkExceptionAndRemoval(): Boolean = {
            reevalVal match {
              case Pulse.Exceptional(f) if onError == null =>
                throw new ObservedException(Event.this, "observed", f)
              case _ => ()
            }
            false
          }
          override def execute(): Unit =
            internalVal match {
              case Pulse.NoChange       => ()
              case Pulse.Value(v)       => onValue(v)
              case Pulse.Exceptional(f) => onError(f)
            }
        }
      }

    /** Uses a partial function `onFailure` to recover an error carried by the event into a value when returning Some(value),
      * or filters the error when returning None
      */
    final def recover[R >: T](onFailure: PartialFunction[Throwable, Option[R]])(implicit
        ticket: CreationTicket[State]
    ): Event[R] =
      Events.staticNamed(s"(recover $this)", this) { st =>
        st.collectStatic(this) match {
          case Exceptional(t) =>
            onFailure.applyOrElse[Throwable, Option[R]](t, throw _).fold[Pulse[R]](Pulse.NoChange)(Pulse.Value(_))
          case other => other
        }
      }

    /** Events disjunction.
      * Propagates the values if any of the events fires.
      * Only propagates the left event if both fire.
      * @group operator
      */
    final def ||[U >: T](other: Event[U])(implicit ticket: CreationTicket[State]): Event[U] = {
      Events.staticNamed(s"(or $this $other)", this, other) { st =>
        val tp = st.collectStatic(this)
        if (tp.isChange) tp else st.collectStatic(other)
      }
    }

    /** Propagates the event only when the other event `exception` does not fire.
      * @group operator
      */
    final def except(exception: Event[Any])(implicit ticket: CreationTicket[State]): Event[T] = {
      Events.staticNamed(s"(except $this  $exception)", this, exception) { st =>
        st.collectStatic(exception) match {
          case NoChange            => st.collectStatic(this)
          case Value(_)            => Pulse.NoChange
          case ex @ Exceptional(_) => ex
        }
      }
    }

    /** Flattens the inner value.
      * @group operator
      */
    final def flatten[R](implicit flatten: Flatten[Event[T], R]): R = flatten.apply(this)

    /** Applies a function on the current value of the signal every time the event occurs,
      * starting with the init value before the first event occurrence
      * @group conversion
      */
    final def iterate[A](init: A)(f: A => A)(implicit ticket: CreationTicket[State]): Signal[A] =
      fold(init)((acc, _) => f(acc))

    /** Counts the occurrences of the event.
      * The argument of the event is discarded.
      * Always starts from 0 when the count is created (no matter how often the event has activated in the past).
      * @group conversion
      */
    final def count()(implicit ticket: CreationTicket[State]): Signal[Int] =
      iterate(0)(_ + 1)

    /** returns a signal holding the latest value of the event.
      * @param init initial value of the returned signal
      * @group conversion
      */
    final def hold[A >: T](init: A)(implicit ticket: CreationTicket[State]): Signal[A] =
      fold(init)((_, v) => v)

    /** returns a signal holding the latest value of the event.
      * @group conversion
      */
    final def hold[A >: T]()(implicit ticket: CreationTicket[State]): Signal[A] =
      Fold(throw EmptySignalControlThrowable)(this act { v => v })

    /** Holds the latest value of an event as an Option, None before the first event occured
      * @group conversion
      */
    final def holdOption[A >: T]()(implicit ticket: CreationTicket[State]): Signal[Option[A]] =
      fold(Option.empty[A]) { (_, v) => Some(v) }

    /** Returns a signal which holds the last n events in a list. At the beginning the
      * list increases in size up to when n values are available
      * @group conversion
      */
    final def list[A >: T](n: Int)(implicit ticket: CreationTicket[State]): Signal[LinearSeq[A]] = {
      if (n < 0) throw new IllegalArgumentException(s"length must be positive")
      else if (n == 0) Var(Nil)
      else
        fold(Queue[A]()) { (queue: Queue[A], v: T) =>
          if (queue.lengthCompare(n) >= 0) queue.tail.enqueue(v) else queue.enqueue(v)
        }
    }

    /** collects events resulting in a variable holding a list of all values.
      * @group conversion
      */
    final def list[A >: T]()(implicit ticket: CreationTicket[State]): Signal[List[A]] =
      fold(List[A]())((acc, v) => v :: acc)

    /** Switch back and forth between two signals on occurrence of event e
      * @group conversion
      */
    final def toggle[A](a: Signal[A], b: Signal[A])(implicit ticket: CreationTicket[State]): Signal[A] =
      ticket.scope.embedTransaction { ict =>
        val switched: Signal[Boolean] = iterate(false) { !_ }(using ict)
        Signal.dynamic(using ict) { if switched.value then b.value else a.value }
      }

    /** Filters the event, only propagating the value when the filter is true.
      *
      * @group operator
      */
    final inline def filter(inline expression: T => Boolean)(implicit ticket: CreationTicket[State]): Event[T] =
      Event.dynamic {
        this.value.filter(expression)
      }

    /** Filters the event, only propagating the value when the filter is true.
      *
      * @group operator
      */
    final infix inline def &&(inline expression: T => Boolean)(implicit ticket: CreationTicket[State]): Event[T] =
      filter(expression)

    /** Collects the results from a partial function
      *
      * @group operator
      */
    final inline def collect[U](inline expression: PartialFunction[T, U])(implicit
        ticket: CreationTicket[State]
    ): Event[U] =
      Event.dynamic {
        this.value.collect(expression)
      }

    /** Transform the event.
      *
      * @group operator
      */
    final inline def map[B](inline expression: T => B)(implicit ticket: CreationTicket[State]): Event[B] =
      Event.dynamic {
        this.value.map(expression)
      }

    /** Folds events with a given operation to create a Signal.
      *
      * @group conversion
      * @inheritdoc
      */
    final def fold[A](init: A)(op: (A, T) => A)(implicit ticket: CreationTicket[State]): Signal[A] =
      Fold(init)(this act { v => op(current, v) })

  }

  /** Similar to [[Signal]] expressions, but resulting in an event.
    * Accessed events return options depending on whether they fire or not,
    * and the complete result of the expression is an event as well.
    *
    * @see [[Signal]]
    * @group create
    */
  object Event {
    inline def apply[T](inline expr: Option[T])(using ct: CreationTicket[BundleState]): Event[T] = static(expr)

    inline def static[T](inline expr: Option[T])(using ct: CreationTicket[BundleState]): Event[T] = {
      val (sources, fun, isStatic) =
        rescala.macros.getDependencies[Option[T], ReSource.of[BundleState], StaticTicket[BundleState], true](expr)
      Events.static(sources: _*)(fun)
    }

    inline def dynamic[T](inline expr: Option[T])(using ct: CreationTicket[BundleState]): Event[T] = {
      val (sources, fun, isStatic) =
        rescala.macros.getDependencies[Option[T], ReSource.of[BundleState], DynamicTicket[BundleState], false](expr)
      Events.dynamic(sources: _*)(fun)
    }

    /** Allows to call some API that requires a callback.
      * {{{
      * val toggle = Event.fromCallback[UIEvent] {
      *   input(`type` := "checkbox",
      *         onchange := Event.handle)
      * }
      * }}}
      */
    def fromCallback[T]: Events.FromCallback[T] = Events.FromCallback[T]()

    /** The callback available within `fromCallback` */
    def handle[T](using cbt: Events.CallBackTrigger[T], scheduler: Scheduler[BundleState])(v: T): Unit =
      Events.CallBackTrigger.unwrap(cbt).fire(v)

  }

  object Events {

    /** the basic method to create static events */
    def staticNamed[T](
        name: String,
        dependencies: ReSource.of[BundleState]*
    )(expr: StaticTicket[BundleState] => Pulse[T])(implicit
        ticket: CreationTicket[BundleState],
        info: ReInfo
    ): Event[T] = {
      ticket.create[Pulse[T], EventImpl[BundleState, T] with Event[T]](
        dependencies.toSet,
        Pulse.NoChange,
        needsReevaluation = false
      ) {
        state => new EventImpl(state, expr, info.derive(name), None) with Event[T]
      }
    }

    /** Creates static events */
    def static[T](dependencies: ReSource.of[BundleState]*)(expr: StaticTicket[BundleState] => Option[T])(implicit
        ticket: CreationTicket[BundleState]
    ): Event[T] =
      staticNamed(ticket.info.description, dependencies: _*)(st => Pulse.fromOption(expr(st)))(ticket, ticket.info)

    /** Creates dynamic events */
    def dynamic[T](dependencies: ReSource.of[BundleState]*)(expr: DynamicTicket[BundleState] => Option[T])(implicit
        ticket: CreationTicket[BundleState]
    ): Event[T] = {
      val staticDeps = dependencies.toSet
      ticket.create[Pulse[T], EventImpl[BundleState, T] with Event[T]](
        staticDeps,
        Pulse.NoChange,
        needsReevaluation = true
      ) {
        state =>
          new EventImpl(state, expr.andThen(Pulse.fromOption), ticket.info, Some(staticDeps)) with Event[T]
      }
    }

    /** Creates change events */
    def change[T](signal: Signal[T])(implicit ticket: CreationTicket[BundleState]): Event[Diff[T]] =
      ticket.scope.embedTransaction { tx =>
        val internal = tx.initializer.create[(Pulse[T], Pulse[Diff[T]]), ChangeEventImpl[BundleState, T]
          with Event[Diff[T]]](
          Set[ReSource.of[BundleState]](signal),
          (Pulse.NoChange, Pulse.NoChange),
          needsReevaluation = true
        ) { state =>
          new ChangeEventImpl(state, signal, ticket.info) with Event[Diff[T]]
        }
        static(internal)(st => st.dependStatic(internal))(tx)
      }

    case class CBR[T, R](event: Event[T], data: R)
    opaque type CallBackTrigger[T] = Evt[T]

    class FromCallback[T]() {
      def apply[R](using CreationTicket[BundleState])(block: Events.CallBackTrigger[T] ?=> R): Events.CBR[T, R] =
        val evt = Evt[T]()
        val res = block(using Events.CallBackTrigger.wrap(evt))
        Events.CBR(evt, res)
    }
    object CallBackTrigger {
      def wrap[T](evt: Evt[T]): CallBackTrigger[T]               = evt
      def unwrap[T](callBackTrigger: CallBackTrigger[T]): Evt[T] = callBackTrigger
    }

  }

}
