package rescala.operator

import rescala.core._
import rescala.interface.RescalaInterface
import rescala.operator.Pulse.{Exceptional, NoChange, Value}
import rescala.operator.RExceptions.ObservedException
import rescala.operator.DefaultImplementations

import scala.collection.immutable.{LinearSeq, Queue}

trait EventApi {
  self: RescalaInterface =>

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
    * @tparam S Internal [[rescala.core.Struct]]ure of state.
    *
    * @groupname operator Event operators
    * @groupprio operator 10
    * @groupname conversion Event to Signal conversions
    * @groupprio conversion 20
    * @groupname accessors Accessors and observers
    * @groupprio accessor 5
    */
  trait Event[+T] extends ReSource with Interp[Option[T]] with Disconnectable {

    implicit def internalAccess(v: Value): Pulse[T]

    /** Interprets the pulse of the event by converting to an option
      * @group internal
      */
    override def interpret(v: Value): Option[T] = v.toOption

    /** Adds an observer.
      * @usecase def +=(handler: T => Unit): Observe
      * @see observe
      * @group accessor
      */
    final def +=(handler: T => Unit)(implicit ticket: CreationTicket): Observe = observe(handler)(ticket)

    /** Add an observer.
      *
      * @return the resulting [[Observe]] can be used to remove the observer.
      * @usecase def observe(handler: T => Unit): Observe
      * @group accessor
      */
    final def observe(onValue: T => Unit, onError: Throwable => Unit = null, fireImmediately: Boolean = false)(implicit
        ticket: CreationTicket
    ): Observe =
      Observe.strong(this, fireImmediately) { reevalVal =>
        val internalVal = internalAccess(reevalVal)
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
      * @usecase def recover[R >: T](onFailure: PartialFunction[Throwable, Option[R]]): rescala.default.Event[R]
      */
    final def recover[R >: T](onFailure: PartialFunction[Throwable, Option[R]])(implicit
        ticket: CreationTicket
    ): Event[R] =
      Events.staticNamed(s"(recover $this)", this) { st =>
        st.collectStatic(this) match {
          case Exceptional(t) =>
            onFailure.applyOrElse[Throwable, Option[R]](t, throw _).fold[Pulse[R]](Pulse.NoChange)(Pulse.Value(_))
          case other => other
        }
      }

    /** Collects the results from a partial function
      * @usecase def collect[U](pf: PartialFunction[T, U]): rescala.default.Event[U]
      * @group operator
      */
    final def collect[U](expression: PartialFunction[T, U])(implicit ticket: CreationTicket): Event[U] =
      Events.staticNamed(s"(collect $this)", this) { st => st.collectStatic(this).collect(expression) }

    /** Events disjunction.
      * Propagates the values if any of the events fires.
      * Only propagates the left event if both fire.
      * @usecase def ||[U >: T](other: rescala.default.Event[U]): rescala.default.Event[U]
      * @group operator
      */
    @cutOutOfUserComputation
    final def ||[U >: T](other: Event[U])(implicit ticket: CreationTicket): Event[U] = {
      Events.staticNamed(s"(or $this $other)", this, other) { st =>
        val tp = st.collectStatic(this)
        if (tp.isChange) tp else other.internalAccess(st.collectStatic(other))
      }
    }



    /** Propagates the event only when except does not fire.
      * @usecase def \[U](except: rescala.default.Event[U]): rescala.default.Event[T]
      * @group operator
      */
    @cutOutOfUserComputation
    final def \[U](except: Event[U])(implicit ticket: CreationTicket): Event[T] = {
      Events.staticNamed(s"(except $this  $except)", this, except) { st =>
        (except.internalAccess(st.collectStatic(except)): Pulse[U]) match {
          case NoChange            => st.collectStatic(this)
          case Value(_)            => Pulse.NoChange
          case ex @ Exceptional(_) => ex
        }
      }
    }

    /** Merge the event with the other, if both fire simultaneously.
      * @usecase def and[U, R](other: rescala.default.Event[U]): rescala.default.Event[R]
      * @group operator
      */
    @cutOutOfUserComputation
    final def and[U, R](other: Event[U])(merger: (T, U) => R)(implicit ticket: CreationTicket): Event[R] = {
      Events.staticNamed(s"(and $this $other)", this, other) { st =>
        for {
          left  <- internalAccess(st.collectStatic(this)): Pulse[T]
          right <- other.internalAccess(st.collectStatic(other)): Pulse[U]
        } yield { merger(left, right) }
      }
    }

    /** Merge the event with the other into a tuple, if both fire simultaneously.
      * @usecase def zip[U](other: rescala.default.Event[U]): rescala.default.Event[(T, U)]
      * @group operator
      * @see and
      */
    @cutOutOfUserComputation
    final def zip[U](other: Event[U])(implicit ticket: CreationTicket): Event[(T, U)] = and(other)(Tuple2.apply)

    /** Merge the event with the other into a tuple, even if only one of them fired.
      * @usecase def zipOuter[U](other: Event[U]): rescala.default.Event[(Option[T], Option[U])]
      * @group operator
      */
    @cutOutOfUserComputation
    final def zipOuter[U](other: Event[U])(implicit ticket: CreationTicket): Event[(Option[T], Option[U])] = {
      Events.staticNamed(s"(zipOuter $this $other)", this, other) { st =>
        val left: Pulse[T]  = st.collectStatic(this)
        val right: Pulse[U] = other.internalAccess(st.collectStatic(other))
        if (right.isChange || left.isChange) Value(left.toOption -> right.toOption) else NoChange
      }
    }


    /** Drop the event parameter; equivalent to map((_: Any) => ())
      * @usecase def dropParam(implicit ticket: CreationTicket): rescala.default.Event[Unit]
      * @group operator
      */
    @cutOutOfUserComputation
    final def dropParam(implicit ticket: CreationTicket): Event[Unit] =
      Events.static(this)(_ => Some(()))


    /** reduces events with a given reduce function to create a Signal
      *
      * @usecase def reduce[A](reducer: (=> A, => T) => A): rescala.default.Signal[A]
      * @group conversion
      */
    @cutOutOfUserComputation
    final def reduce[A](reducer: (=> A, => T) => A)(implicit ticket: CreationTicket): Signal[A] = {
      val res = ticket.create(
        Set(this),
        Pulse.empty: Pulse[A],
        inite = false
      ) { state =>
        new SignalImpl[A](
          initial = state,
          expr = { (st, currentValue) => reducer(currentValue(), st.collectStatic(this).get) },
          name = ticket.rename,
          isDynamicWithStaticDeps = None
        )
      }
      Signals.wrapWithSignalAPI(res)
    }

    /** Applies a function on the current value of the signal every time the event occurs,
      * starting with the init value before the first event occurrence
      * @usecase def iterate[A](init: A)(f: A => A): rescala.default.Signal[A]
      * @group conversion
      */
    @cutOutOfUserComputation
    final def iterate[A](init: A)(f: A => A)(implicit ticket: CreationTicket): Signal[A] =
      Events.foldOne(this, init)((acc, _) => f(acc))

    /** Counts the occurrences of the event. Starts from 0, when the event has never been
      * fired yet. The argument of the event is simply discarded.
      * @group conversion
      * @usecase def count(): rescala.default.Signal[Int]
      * @inheritdoc
      */
    @cutOutOfUserComputation
    final def count()(implicit ticket: CreationTicket): Signal[Int] =
      Events.foldOne(this, 0)((acc, _) => acc + 1)

    /** returns a signal holding the latest value of the event.
      * @param init initial value of the returned signal
      * @usecase def latest[A >: T](init: A): rescala.default.Signal[A]
      * @group conversion
      */
    @cutOutOfUserComputation
    final def latest[A >: T](init: A)(implicit ticket: CreationTicket): Signal[A] =
      Events.foldOne(this, init)((_, v) => v)

    /** returns a signal holding the latest value of the event.
      * @usecase def latest[A >: T](): rescala.default.Signal[A]
      * @group conversion
      */
    @cutOutOfUserComputation
    final def latest[A >: T]()(implicit ticket: CreationTicket): Signal[A] =
      reduce[A]((_, v) => v)

    /** Holds the latest value of an event as an Option, None before the first event occured
      * @usecase def latestOption[A >: T](): rescala.default.Signal[Option[A]]
      * @group conversion
      */
    @cutOutOfUserComputation
    final def latestOption[A >: T]()(implicit ticket: CreationTicket): Signal[Option[A]] =
      Events.foldOne(this, None: Option[A]) { (_, v) => Some(v) }

    /** Returns a signal which holds the last n events in a list. At the beginning the
      * list increases in size up to when n values are available
      * @usecase def last[A >: T](n: Int): rescala.default.Signal[LinearSeq[A]]
      * @group conversion
      */
    @cutOutOfUserComputation
    final def last[A >: T](n: Int)(implicit ticket: CreationTicket): Signal[LinearSeq[A]] = {
      if (n < 0) throw new IllegalArgumentException(s"length must be positive")
      else if (n == 0) Var(Nil)
      else
        Events.foldOne(this, Queue[A]()) { (queue: Queue[A], v: T) =>
          if (queue.lengthCompare(n) >= 0) queue.tail.enqueue(v) else queue.enqueue(v)
        }
    }

    /** collects events resulting in a variable holding a list of all values.
      * @usecase def list[A >: T](): rescala.default.Signal[List[A]]
      * @group conversion
      */
    @cutOutOfUserComputation
    final def list[A >: T]()(implicit ticket: CreationTicket): Signal[List[A]] =
      Events.foldOne(this, List[A]())((acc, v) => v :: acc)

    /** Switch back and forth between two signals on occurrence of event e
      * @usecase def toggle[A](a: rescala.default.Signal[A], b: rescala.default.Signal[A]): rescala.default.Signal[A]
      * @group conversion
      */
    @cutOutOfUserComputation
    final def toggle[A](a: Signal[A], b: Signal[A])(implicit ticket: CreationTicket): Signal[A] =
      ticket.transaction { ict =>
        val switched: Signal[Boolean] = iterate(false) { !_ }(ict)
        Signals.dynamic(switched, a, b) { s => if (s.depend(switched)) s.depend(b) else s.depend(a) }(ict)
      }

  }

  object Events {

    /** the basic method to create static events */
    @cutOutOfUserComputation
    def staticNamed[T](name: String, dependencies: ReSource*)(calculate: StaticTicket => Pulse[T])(implicit
        ticket: CreationTicket
    ): Event[T] = {
      ticket.create[Pulse[T], EventImpl[T]](dependencies.toSet, Pulse.NoChange, inite = false) {
        state => new EventImpl[T](state, calculate, name, None)
      }
    }

    /** Creates static events */
    @cutOutOfUserComputation
    def static[T](dependencies: ReSource*)(calculate: StaticTicket => Option[T])(implicit
        ticket: CreationTicket
    ): Event[T] =
      staticNamed(ticket.rename.str, dependencies: _*)(st => Pulse.fromOption(calculate(st)))

    /** Creates dynamic events */
    @cutOutOfUserComputation
    def dynamic[T](dependencies: ReSource*)(expr: DynamicTicket => Option[T])(implicit
        ticket: CreationTicket
    ): Event[T] = {
      val staticDeps = dependencies.toSet
      ticket.create[Pulse[T], EventImpl[T]](staticDeps, Pulse.NoChange, inite = true) { state =>
        new EventImpl[T](state, expr.andThen(Pulse.fromOption), ticket.rename, Some(staticDeps))
      }
    }

    /** Creates change events */
    @cutOutOfUserComputation
    def change[T](signal: Signal[T])(implicit ticket: CreationTicket): Event[Diff[T]] =
      ticket.transaction { initTurn =>
        val internal = initTurn.create[(Pulse[T], Pulse[Diff[T]]), ChangeEventImpl[T]](
          Set[ReSource](signal),
          (Pulse.NoChange, Pulse.NoChange),
          inite = true,
          ticket
        ) { state =>
          new ChangeEventImpl[T](state, signal, ticket.rename)
        }
        static(internal)(st => st.dependStatic(internal))(initTurn)
      }

    @cutOutOfUserComputation
    def foldOne[A, T](dependency: Event[A], init: T)(expr: (T, A) => T)(implicit
        ticket: CreationTicket
    ): Signal[T] = {
      fold(Set[ReSource](dependency), init) { st => acc =>
        val a: A = dependency.internalAccess(st.collectStatic(dependency)).get
        expr(acc(), a)
      }
    }

    /** Folds events with a given operation to create a Signal.
      *
      * @see [[rescala.operator.Event.fold]]
      */
    @cutOutOfUserComputation
    def fold[T](dependencies: Set[ReSource], init: T)(expr: StaticTicket => (() => T) => T)(implicit
        ticket: CreationTicket
    ): Signal[T] = {
      val res = ticket.create(
        dependencies,
        Pulse.tryCatch[Pulse[T]](Pulse.Value(init)),
        inite = false
      ) {
        state => new SignalImpl[T](state, (st, v) => expr(st)(v), ticket.rename, None)
      }
      Signals.wrapWithSignalAPI(res)
    }

    /** Folds when any one of a list of events occurs, if multiple events occur, every fold is executed in order. */
    @cutOutOfUserComputation
    final def foldAll[A](init: A)(accthingy: (=> A) => Seq[FoldMatch[A]])(implicit
        ticket: CreationTicket
    ): Signal[A] = {
      var acc = () => init
      val ops = accthingy(acc())
      val staticInputs = ops.collect {
        case StaticFoldMatch(ev, _)        => ev
        case StaticFoldMatchDynamic(ev, _) => ev
      }.toSet

      def operator(dt: DynamicTicket, oldValue: () => A): A = {
        acc = oldValue

        def applyToAcc[T](f: T => A, value: Option[T]): Unit = {
          value.foreach { v =>
            val res = f(v)
            acc = () => res
          }
        }

        ops.foreach {
          case StaticFoldMatch(ev, f) =>
            applyToAcc(f, dt.dependStatic(ev))
          case StaticFoldMatchDynamic(ev, f) =>
            applyToAcc(f(dt), dt.dependStatic(ev))
          case DynamicFoldMatch(evs, f) =>
            evs().map(dt.depend).foreach { applyToAcc(f, _) }
        }
        acc()
      }

      val res = ticket.create(
        staticInputs.toSet[ReSource],
        Pulse.tryCatch[Pulse[A]](Pulse.Value(init)),
        inite = true
      ) {
        state => new SignalImpl[A](state, operator, ticket.rename, Some(staticInputs.toSet[ReSource]))
      }
      Signals.wrapWithSignalAPI(res)

    }

    val Match = Seq

    sealed trait FoldMatch[+A]
    case class StaticFoldMatch[T, +A](event: Event[T], f: T => A)                         extends FoldMatch[A]
    case class StaticFoldMatchDynamic[T, +A](event: Event[T], f: DynamicTicket => T => A) extends FoldMatch[A]
    case class DynamicFoldMatch[T, +A](event: () => Seq[Event[T]], f: T => A)             extends FoldMatch[A]

    class OnEv[T](e: Event[T]) {

      /** Constructs a pair similar to ->, however this one is compatible with type inference for [[fold]] */
      final def act[A](fun: T => A): FoldMatch[A]                  = StaticFoldMatch(e, fun)
      final def dyn[A](fun: DynamicTicket => T => A): FoldMatch[A] = StaticFoldMatchDynamic(e, fun)
    }
    class OnEvs[T](e: => Seq[Event[T]]) {

      /** Constructs a pair similar to ->, however this one is compatible with type inference for [[fold]] */
      final def act[A](fun: T => A): FoldMatch[A] = DynamicFoldMatch(() => e, fun)
    }

    case class CBResult[T, R](event: Event[T], value: R)
    final class FromCallbackT[T] private[Events] (val dummy: Boolean = true) {
      def apply[R](body: (T => Unit) => R)(implicit ct: CreationTicket, s: Scheduler): CBResult[T, R] = {
        val evt: self.Evt[T] = self.Evt[T]()(ct)
        val res = body(evt.fire(_)(s))
        CBResult(evt, res)
      }
    }

    def fromCallback[T]: FromCallbackT[T] = new FromCallbackT[T]()
  }

}
