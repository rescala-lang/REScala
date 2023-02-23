package rescala.operator

import rescala.compat.EventCompatBundle
import rescala.core.{Disconnectable, ReInfo, ReSource, ReadAs, Scheduler, StaticTicket}
import rescala.operator.Pulse.{Exceptional, NoChange, Value}
import rescala.operator.RExceptions.ObservedException

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.immutable.{LinearSeq, Queue}

object EventsMacroImpl {
  object MapFuncImpl {
    def apply[T1, A](value: Option[T1], mapper: T1 => A): Option[A] =
      value.map(mapper)
  }
  object FilterFuncImpl {
    def apply[T1, A](value: Option[T1], filter: T1 => Boolean): Option[T1] =
      value.filter(filter)
  }
  object CollectFuncImpl {
    def apply[T1, A](value: Option[T1], filter: PartialFunction[T1, A]): Option[A] =
      value.collect(filter)
  }
  object FoldFuncImpl {
    def apply[T1, A](state: () => A, value: Option[T1], step: (A, T1) => A): A =
      value match {
        case None    => state()
        case Some(v) => step(state(), v)
      }
  }

}

trait EventBundle extends EventCompatBundle {
  selfType: Operators =>

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
  trait Event[+T] extends EventCompat[T] with Disconnectable {
    final override type State[V] = selfType.State[V]

    implicit def internalAccess(v: Value): Pulse[T]
    def resource: ReadAs.of[State, Option[T @uncheckedVariance]] = this

    /** Interprets the pulse of the event by converting to an option
      *
      * @group internal
      */
    override def read(v: Value): Option[T] = v.toOption

    /** Adds an observer.
      * @see observe
      * @group accessor
      */
    final def +=(handler: T => Unit)(implicit ticket: CreationTicket): Disconnectable = observe(handler)(ticket)

    /** Add an observer.
      *
      * @return the resulting [[rescala.operator.ObserveBundle.Observe]] can be used to remove the observer.
      * @group accessor
      */
    final def observe(onValue: T => Unit, onError: Throwable => Unit = null, fireImmediately: Boolean = false)(implicit
        ticket: CreationTicket
    ): Disconnectable =
      Observe.strong(this, fireImmediately) { reevalVal =>
        val internalVal = internalAccess(reevalVal)
        new ObserveInteract {
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
        ticket: CreationTicket
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
    @cutOutOfUserComputation
    final def ||[U >: T](other: Event[U])(implicit ticket: CreationTicket): Event[U] = {
      Events.staticNamed(s"(or $this $other)", this, other) { st =>
        val tp = st.collectStatic(this)
        if (tp.isChange) tp else other.internalAccess(st.collectStatic(other))
      }
    }

    /** Propagates the event only when except does not fire.
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
      * @group operator
      * @see and
      */
    @cutOutOfUserComputation
    final def zip[U](other: Event[U])(implicit ticket: CreationTicket): Event[(T, U)] = and(other)(Tuple2.apply)

    /** Merge the event with the other into a tuple, even if only one of them fired.
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

    /** Flattens the inner value.
      * @group operator
      */
    @cutOutOfUserComputation
    final def flatten[R](implicit flatten: Flatten[Event[T], R]): R = flatten.apply(this)

    /** Drop the event parameter; equivalent to map((_: Any) => ())
      * @group operator
      */
    @cutOutOfUserComputation
    final def dropParam(implicit ticket: CreationTicket): Event[Unit] =
      Events.static(this)(_ => Some(()))

    /** reduces events with a given reduce function to create a Signal
      *
      * @group conversion
      */
    @cutOutOfUserComputation
    final def reduce[A](reducer: (=> A, => T) => A)(implicit ticket: CreationTicket): Signal[A] = {
      ticket.create(
        Set(this),
        Pulse.empty: Pulse[A],
        needsReevaluation = false
      ) { state =>
        new SignalImpl[A](
          initial = state,
          expr = { (st, currentValue) => reducer(currentValue(), st.collectStatic(this).get) },
          name = ticket.rename,
          isDynamicWithStaticDeps = None
        )
      }
    }

    /** Applies a function on the current value of the signal every time the event occurs,
      * starting with the init value before the first event occurrence
      * @group conversion
      */
    @cutOutOfUserComputation
    final def iterate[A](init: A)(f: A => A)(implicit ticket: CreationTicket): Signal[A] =
      Events.foldOne(this, init)((acc, _) => f(acc))

    /** Counts the occurrences of the event.
      * The argument of the event is discarded.
      * Always starts from 0 when the count is created (no matter how often the event has activated in the past).
      * @group conversion
      */
    @cutOutOfUserComputation
    final def count()(implicit ticket: CreationTicket): Signal[Int] =
      Events.foldOne(this, 0)((acc, _) => acc + 1)

    /** returns a signal holding the latest value of the event.
      * @param init initial value of the returned signal
      * @group conversion
      */
    @cutOutOfUserComputation
    final def latest[A >: T](init: A)(implicit ticket: CreationTicket): Signal[A] =
      Events.foldOne(this, init)((_, v) => v)

    /** returns a signal holding the latest value of the event.
      * @group conversion
      */
    @cutOutOfUserComputation
    final def latest[A >: T]()(implicit ticket: CreationTicket): Signal[A] =
      reduce[A]((_, v) => v)

    /** Holds the latest value of an event as an Option, None before the first event occured
      * @group conversion
      */
    @cutOutOfUserComputation
    final def latestOption[A >: T]()(implicit ticket: CreationTicket): Signal[Option[A]] =
      Events.foldOne(this, None: Option[A]) { (_, v) => Some(v) }

    /** Returns a signal which holds the last n events in a list. At the beginning the
      * list increases in size up to when n values are available
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
      * @group conversion
      */
    @cutOutOfUserComputation
    final def list[A >: T]()(implicit ticket: CreationTicket): Signal[List[A]] =
      Events.foldOne(this, List[A]())((acc, v) => v :: acc)

    /** Switch back and forth between two signals on occurrence of event e
      * @group conversion
      */
    @cutOutOfUserComputation
    final def toggle[A](a: Signal[A], b: Signal[A])(implicit ticket: CreationTicket): Signal[A] =
      ticket.scope.embedTransaction { ict =>
        val switched: Signal[Boolean] = iterate(false) { !_ }(ict)
        Signals.dynamic(switched, a, b) { s => if (s.depend(switched)) s.depend(b) else s.depend(a) }(ict)
      }

  }

  object Events {

    /** the basic method to create static events */
    @cutOutOfUserComputation
    def staticNamed[T](name: String, dependencies: ReSource.of[State]*)(expr: StaticTicket[State] => Pulse[T])(implicit
                                                                                                               ticket: CreationTicket,
                                                                                                               info: ReInfo
    ): Event[T] = {
      ticket.create[Pulse[T], EventImpl[T]](dependencies.toSet, Pulse.NoChange, needsReevaluation = false) {
        state => new EventImpl[T](state, expr, info.derive(name), None)
      }
    }

    /** Creates static events */
    @cutOutOfUserComputation
    def static[T](dependencies: ReSource.of[State]*)(expr: StaticTicket[State] => Option[T])(implicit
        ticket: CreationTicket
    ): Event[T] =
      staticNamed(ticket.rename.description, dependencies: _*)(st => Pulse.fromOption(expr(st)))

    /** Creates static events */
    @cutOutOfUserComputation
    def staticNoVarargs[T](dependencies: Seq[ReSource.of[State]])(expr: StaticTicket[State] => Option[T])(implicit
        ticket: CreationTicket
    ): Event[T] = static(dependencies: _*)(expr)

    /** Creates dynamic events */
    @cutOutOfUserComputation
    def dynamic[T](dependencies: ReSource.of[State]*)(expr: DynamicTicket => Option[T])(implicit
        ticket: CreationTicket
    ): Event[T] = {
      val staticDeps = dependencies.toSet
      ticket.create[Pulse[T], EventImpl[T]](staticDeps, Pulse.NoChange, needsReevaluation = true) { state =>
        new EventImpl[T](state, expr.andThen(Pulse.fromOption), ticket.rename, Some(staticDeps))
      }
    }

    /** Creates dynamic events */
    @cutOutOfUserComputation
    def dynamicNoVarargs[T](dependencies: Seq[ReSource.of[State]])(expr: DynamicTicket => Option[T])(implicit
        ticket: CreationTicket
    ): Event[T] = dynamic(dependencies: _*)(expr)

    /** Creates change events */
    @cutOutOfUserComputation
    def change[T](signal: Signal[T])(implicit ticket: CreationTicket): Event[Diff[T]] =
      ticket.scope.embedTransaction { tx =>
        val internal = tx.initializer.create[(Pulse[T], Pulse[Diff[T]]), ChangeEventImpl[T]](
          Set[ReSource.of[State]](signal),
          (Pulse.NoChange, Pulse.NoChange),
          needsReevaluation = true
        ) { state =>
          new ChangeEventImpl[T](state, signal, ticket.rename)
        }
        static(internal)(st => st.dependStatic(internal))(tx)
      }

    @cutOutOfUserComputation
    def foldOne[A, T](dependency: Event[A], init: T)(expr: (T, A) => T)(implicit
        ticket: CreationTicket
    ): Signal[T] = {
      fold(Set(dependency), init) { st => acc =>
        val a: A = dependency.internalAccess(st.collectStatic(dependency)).get
        expr(acc(), a)
      }
    }

    /** Folds events with a given operation to create a Signal.
      *
      * @see [[rescala.operator.EventBundle.Event.fold]]
      */
    @cutOutOfUserComputation
    def fold[T](dependencies: Set[ReSource.of[State]], init: T)(expr: DynamicTicket => (() => T) => T)(implicit
        ticket: CreationTicket
    ): Signal[T] = {
      ticket.create(
        dependencies,
        Pulse.tryCatch[T](Pulse.Value(init)),
        needsReevaluation = false
      ) {
        state => new SignalImpl[T](state, (st, v) => expr(st)(v), ticket.rename, None)
      }
    }

    /** Folds when any one of a list of events occurs, if multiple events occur, every fold is executed in order.
      *
      * Example for a counter that can be reset:
      *
      * {{{
      * val add: Event[Int]
      * val reset: Event[Unit]
      * Events.foldAll(0){ current => Seq(
      *   add act2 { v => current + v },
      *   reset act2 { _ => 0 }
      * )}
      * }}}
      */
    @cutOutOfUserComputation
    final def foldAll[A](init: A)(accthingy: (=> A) => Seq[FoldMatch[A]])(implicit
        ticket: CreationTicket
    ): Signal[A] = {
      var acc = () => init
      val ops = accthingy(acc())
      val staticInputs = ops.collect {
        case fm: StaticFoldMatch[_, _]        => fm.event
        case fm: StaticFoldMatchDynamic[_, _] => fm.event
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
          case fm: StaticFoldMatch[_, A] =>
            applyToAcc(fm.f, dt.dependStatic(fm.event))
          case fm: StaticFoldMatchDynamic[_, A] =>
            applyToAcc(fm.f(dt), dt.dependStatic(fm.event))
          case fm: DynamicFoldMatch[_, A] =>
            fm.event().map(dt.depend).foreach { applyToAcc(fm.f, _) }
        }
        acc()
      }

      ticket.create(
        staticInputs.toSet,
        Pulse.tryCatch[A](Pulse.Value(init)),
        needsReevaluation = true
      ) {
        state => new SignalImpl[A](state, operator, ticket.rename, Some(staticInputs.toSet))
      }
    }

    sealed trait FoldMatch[+A]
    class StaticFoldMatch[T, +A](val event: Event[T], val f: T => A)                         extends FoldMatch[A]
    class StaticFoldMatchDynamic[T, +A](val event: Event[T], val f: DynamicTicket => T => A) extends FoldMatch[A]
    class DynamicFoldMatch[T, +A](val event: () => Seq[Event[T]], val f: T => A)             extends FoldMatch[A]

    class OnEv[T](event: Event[T]) {

      /** Constructs a handling branch that handles the static `event`
        * @param fun handler for activations of `event`
        */
      final def act2[A](fun: T => A): FoldMatch[A] = new StaticFoldMatch(event, fun)

      /** Similar to act2, but provides access to a dynamic ticket in `fun` */
      final def dyn2[A](fun: DynamicTicket => T => A): FoldMatch[A] = new StaticFoldMatchDynamic(event, fun)
    }

    class OnEvs[T](events: => Seq[Event[T]]) {

      /** Constructs a handler for all `events`, note that `events` may dynamically compute the events from the current state */
      final def act2[A](fun: T => A): FoldMatch[A] = new DynamicFoldMatch(() => events, fun)
    }

    class CBResult[T, R](val event: Event[T], val data: R)
    final class FromCallbackT[T] private[Events] (val dummy: Boolean = true) {
      def apply[R](body: (T => Unit) => R)(implicit ct: CreationTicket, s: Scheduler[State]): CBResult[T, R] = {
        val evt: Evt[T] = Evt[T]()(ct)
        val res         = body(evt.fire(_))
        new CBResult(evt, res)
      }
    }

    def fromCallback[T]: FromCallbackT[T] = new FromCallbackT[T]()
  }

}
