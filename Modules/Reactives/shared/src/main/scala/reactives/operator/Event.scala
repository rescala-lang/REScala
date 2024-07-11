package reactives.operator

import reactives.SelectedScheduler.State
import reactives.core.*
import reactives.macros.MacroAccess
import reactives.structure.*
import reactives.structure.Pulse.{Exceptional, NoChange, Value}
import reactives.structure.RExceptions.EmptySignalControlThrowable

import scala.collection.immutable.{LinearSeq, Queue}

/** Events only propagate a value when they are changing,
  * when the system is at rest, events have no values.
  *
  * Note: We hide using parameters of the API in the documentation.
  * They are used to ensure correct creation, and you normally do not have to worry about them,
  * except if you accidentally call the using parameter list, in which cas you may get cryptic errors.
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

  type State[V] = reactives.SelectedScheduler.State[V]

  extension (v: Value) def access: Pulse[T]


  /** Interprets the pulse of the event by converting to an option
    *
    * @group internal
    */
  override def read(v: Value): Option[T] = v.access.toOption

  /** Add an observer.
    *
    * @return the resulting [[reactives.structure.Observe]] can be used to remove the observer.
    * @group accessor
    */
  final def observe(onValue: T => Unit, onError: Throwable => Unit = null, fireImmediately: Boolean = false)(
      using ticket: CreationTicket[State]
  ): Disconnectable =
    Observe.strong(this, fireImmediately) { reevalVal => Observe.ObservePulsing(reevalVal.access, this, onValue, onError) }

  /** Uses a partial function `onFailure` to recover an error carried by the event into a value when returning Some(value),
    * or filters the error when returning None
    */
  final def recover[R >: T](onFailure: PartialFunction[Exception, Option[R]])(using
      ticket: CreationTicket[State]
  ): Event[R] =
    Event.Impl.staticNamed(s"(recover $this)", this) { st =>
      st.collectStatic(this) match {
        case Exceptional(t) =>
          onFailure.applyOrElse[Exception, Option[R]](t, throw _).fold[Pulse[R]](Pulse.NoChange)(Pulse.Value(_))
        case other => other.access
      }
    }

  /** Events disjunction.
    * Propagates the values if any of the events fires.
    * Only propagates the left event if both fire.
    * @group operator
    */
  final def ||[U >: T](other: Event[U])(using ticket: CreationTicket[State]): Event[U] = {
    Event.Impl.staticNamed(s"(or $this $other)", this, other) { st =>
      val tp = st.collectStatic(this)
      if tp.access.isChange then tp.access else st.collectStatic(other).access
    }
  }

  /** Propagates the event only when the other event `exception` does not fire.
    * @group operator
    */
  final def except(exception: Event[Any])(using ticket: CreationTicket[State]): Event[T] = {
    Event.Impl.staticNamed(s"(except $this  $exception)", this, exception) { st =>
      st.collectStatic(exception) match {
        case NoChange            => st.collectStatic(this).access
        case Value(_)            => Pulse.NoChange
        case ex @ Exceptional(_) => ex
      }
    }
  }

  /** Flattens the inner value.
    * @group operator
    */
  final def flatten[R](using flatten: Flatten[Event[T], R]): R = flatten.apply(this)

  /** Applies a function on the current value of the signal every time the event occurs,
    * starting with the init value before the first event occurrence
    * @group conversion
    */
  final def iterate[A](init: A)(f: A => A)(using ticket: CreationTicket[State]): Signal[A] =
    fold(init)((acc, _) => f(acc))

  /** Counts the occurrences of the event.
    * The argument of the event is discarded.
    * Always starts from 0 when the count is created (no matter how often the event has activated in the past).
    * @group conversion
    */
  final def count()(using ticket: CreationTicket[State]): Signal[Int] =
    iterate(0)(_ + 1)

  /** returns a signal holding the latest value of the event.
    * @param init initial value of the returned signal
    * @group conversion
    */
  final def hold[A >: T](init: A)(using ticket: CreationTicket[State]): Signal[A] =
    fold(init)((_, v) => v)

  /** returns a signal holding the latest value of the event.
    * @group conversion
    */
  final def hold[A >: T]()(using ticket: CreationTicket[State]): Signal[A] =
    Fold(throw EmptySignalControlThrowable(info))(this branch { v => v })

  /** Holds the latest value of an event as an Option, None before the first event occured
    * @group conversion
    */
  final def holdOption[A >: T]()(using ticket: CreationTicket[State]): Signal[Option[A]] =
    fold(Option.empty[A]) { (_, v) => Some(v) }

  /** Returns a signal which holds the last n events in a list. At the beginning the
    * list increases in size up to when n values are available
    * @group conversion
    */
  final def list[A >: T](n: Int)(using ticket: CreationTicket[State]): Signal[LinearSeq[A]] = {
    if n < 0 then throw new IllegalArgumentException(s"length must be positive")
    else if n == 0 then Var(Nil)
    else
      fold(Queue[A]()) { (queue: Queue[A], v: T) =>
        if queue.lengthCompare(n) >= 0 then queue.tail.enqueue(v) else queue.enqueue(v)
      }
  }

  /** collects events resulting in a variable holding a list of all values.
    * @group conversion
    */
  final def list[A >: T]()(using ticket: CreationTicket[State]): Signal[List[A]] =
    fold(List[A]())((acc, v) => v :: acc)

  /** Switch back and forth between two signals on occurrence of event e
    * @group conversion
    */
  final def toggle[A](a: Signal[A], b: Signal[A])(using ticket: CreationTicket[State]): Signal[A] =
    ticket.scope.embedCreation { ict ?=>
      val switched: Signal[Boolean] = iterate(false) { !_ }
      Signal.static { if switched.value then b.value else a.value }
    }

  /** Filters the event, only propagating the value when the filter is true.
    *
    * @group operator
    */
  final inline def filter(inline expression: T => Boolean)(using ticket: CreationTicket[State]): Event[T] =
    Event.static { this.value.filter(expression) }

  /** Filters the event, only propagating the value when the filter is true.
    *
    * @group operator
    */
  final inline def &&(inline expression: T => Boolean)(using ticket: CreationTicket[State]): Event[T] =
    filter(expression)

  /** Collects the results from a partial function
    *
    * @group operator
    */
  final inline def collect[U](inline expression: PartialFunction[T, U])(using CreationTicket[State]): Event[U] =
    Event.static { this.value.collect(expression) }

  /** Transform the event.
    *
    * @group operator
    */
  final inline def map[B](inline expression: T => B)(using ticket: CreationTicket[State]): Event[B] =
    Event.static { this.value.map(expression) }

  /** Like map, but allows to ignore the parameter if its type is Unit.
    * Useful for snapshotting some signals when an event triggers
    *
    * @group operator
    */
  final inline def snap[B, T1 >: T](inline expression: B)(using CreationTicket[State])(using T1 =:= Unit): Event[B] =
    Event.static { Some(expression) }

  /** Folds events with a given operation to create a Signal.
    *
    * @group conversion
    * @inheritdoc
    */
  final inline def fold[A](init: A)(inline op: (A, T) => A)(using ticket: CreationTicket[State]): Signal[A] =
    Fold(init)(
      this.branch: v =>
        op(Fold.current, v)
    )

  /** This creates a branch that can be combined into a `Fold` */
  final inline def branch[S](inline f: FoldState[S] ?=> T => S): Fold.Branch[S] =
    Fold.branch {
      this.value.fold(Fold.current)(f)
    }
}

/** Similar to [[reactives.operator.Signal]] expressions, but resulting in an event.
  * Accessed events return options depending on whether they fire or not,
  * and the complete result of the expression is an event as well.
  *
  * @see [[reactives.operator.Signal]]
  * @group create
  */
object Event {
  inline def apply[T](inline expr: Option[T])(using ct: CreationTicket[State]): Event[T] = static(expr)

  inline def static[T](inline expr: Option[T])(using ct: CreationTicket[State]): Event[T] = {
    val (sources, fun, isStatic) =
      reactives.macros.MacroLegos.getDependencies[Option[T], ReSource.of[State], StaticTicket[State], true](expr)
    Event.Impl.static(sources*)(fun)
  }

  inline def dynamic[T](inline expr: Option[T])(using ct: CreationTicket[State]): Event[T] = {
    val (sources, fun, isStatic) =
      reactives.macros.MacroLegos.getDependencies[Option[T], ReSource.of[State], DynamicTicket[State], false](expr)
    Event.Impl.dynamic(sources*)(fun)
  }

  /** Allows to call some API that requires a callback.
    * {{{
    * val toggle = Event.fromCallback {
    *   input(`type` := "checkbox",
    *         onchange := Event.handle[UIEvent])
    * }
    * }}}
    */
  def fromCallback[R, T](using CreationTicket[State])(block: Accepts[T] ?=> R): CBR[T, R] =
    val evt = Evt[T]()
    val res = block(using evt)
    CBR(evt, res)

  case class CBR[T, R](event: Event[T], data: R)
  opaque type Accepts[T] = Evt[T]

  /** The callback available within `fromCallback` */
  def handle[T](using cbt: Accepts[T], scope: PlanTransactionScope[State])(v: T): Unit = cbt.fire(v)

  object Impl {

    /** the basic method to create static events */
    def staticNamed[T](
        name: String,
        dependencies: ReSource.of[State]*
    )(expr: StaticTicket[State] => Pulse[T])(using
        ticket: CreationTicket[State]
    ): Event[T] = {
      ticket.scope.create[Pulse[T], EventImpl[T] & Event[T]](
        dependencies.toSet,
        Pulse.NoChange,
        needsReevaluation = false
      ) {
        state => new EventImpl(state, expr, ticket.info.derive(name), None) with Event[T]
      }
    }

    /** Creates static events */
    def static[T](dependencies: ReSource.of[State]*)(expr: StaticTicket[State] => Option[T])(using
        ticket: CreationTicket[State]
    ): Event[T] =
      staticNamed(ticket.info.description, dependencies*)(st => Pulse.fromOption(expr(st)))

    /** Creates dynamic events */
    def dynamic[T](dependencies: ReSource.of[State]*)(expr: DynamicTicket[State] => Option[T])(using
        ticket: CreationTicket[State]
    ): Event[T] = {
      val staticDeps = dependencies.toSet
      ticket.scope.create[Pulse[T], EventImpl[T] & Event[T]](
        staticDeps,
        Pulse.NoChange,
        needsReevaluation = true
      ) {
        state =>
          new EventImpl(state, expr.andThen(Pulse.fromOption), ticket.info, Some(staticDeps)) with Event[T]
      }
    }

    /** Creates change events */
    def change[T](signal: Signal[T])(using ticket: CreationTicket[State]): Event[Diff[T]] =
      ticket.scope.embedCreation { tx ?=>
        val internal = tx.initializer.create[(Pulse[T], Pulse[Diff[T]]), ChangeEventImpl[T]
        & Event[Diff[T]]](
          Set[ReSource.of[State]](signal),
          (Pulse.NoChange, Pulse.NoChange),
          needsReevaluation = true
        ) { state =>
          new ChangeEventImpl(state, signal, ticket.info) with Event[Diff[T]]
        }
        static(internal)(st => st.dependStatic(internal))
      }

  }
}
