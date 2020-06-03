package rescala.reactives

import rescala.core._
import rescala.interface.RescalaInterface
import rescala.macros.cutOutOfUserComputation
import rescala.reactives.Signals.Diff


object Events {
  type Estate[S <: Struct, T] = S#State[Pulse[T], S]


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

/** Functions to construct events, you probably want to use the operators on [[Event]] for a nicer API. */
trait Events[S <: Struct] {

  val rescalaAPI: RescalaInterface[S]

  import rescalaAPI.Signal
  import rescalaAPI.Impls._

  /** the basic method to create static events */
  @cutOutOfUserComputation
  def staticNamed[T](name: String,
                     dependencies: ReSource[S]*)
                    (calculate: StaticTicket[S] => Pulse[T])
                    (implicit ticket: CreationTicket[S]): Event[T, S] = {
    ticket.create[Pulse[T], EventImpl[T]](dependencies.toSet, Initializer.Event, inite = false) {
      state => new EventImpl[T](state, calculate, name, None, rescalaAPI)
    }
  }

  /** Creates static events */
  @cutOutOfUserComputation
  def static[T](dependencies: ReSource[S]*)
               (calculate: StaticTicket[S] => Option[T])
               (implicit ticket: CreationTicket[S]): Event[T, S] =
    staticNamed(ticket.rename.str, dependencies: _*)(st => Pulse.fromOption(calculate(st)))

  /** Creates dynamic events */
  @cutOutOfUserComputation
  def dynamic[T](dependencies: ReSource[S]*)
                (expr: DynamicTicket[S] => Option[T])
                (implicit ticket: CreationTicket[S]): Event[T, S] = {
    val staticDeps = dependencies.toSet
    ticket.create[Pulse[T], EventImpl[T]](staticDeps, Initializer.Event, inite = true) { state =>
      new EventImpl[T](state, expr.andThen(Pulse.fromOption), ticket.rename, Some(staticDeps), rescalaAPI)
    }
  }


  /** Creates change events */
  @cutOutOfUserComputation
  def change[T](signal: rescala.reactives.Signal[T, S])(implicit ticket: CreationTicket[S]): Event[Diff[T], S]
  = ticket.transaction { initTurn =>
    val internal = initTurn.create[(Pulse[T], Pulse[Diff[T]]), ChangeEventImpl[T]](
      Set[ReSource[S]](signal), Initializer.Change, inite = true, ticket) { state =>
      new ChangeEventImpl[T](state, signal, ticket.rename, rescalaAPI)
    }
    static(internal)(st => st.dependStatic(internal))(initTurn)
  }

  @cutOutOfUserComputation
  def foldOne[A, T](dependency: Event[A, S], init: T)(expr: (T, A) => T)(implicit ticket: CreationTicket[S]): Signal[T] = {
    fold(Set[ReSource[S]](dependency), init) { st =>
      acc =>
        val a: A = dependency.internalAccess(st.collectStatic(dependency)).get
        expr(acc(), a)
    }
  }

  /** Folds events with a given operation to create a Signal.
    *
    * @see [[rescala.reactives.Event.fold]]*/
  @cutOutOfUserComputation
  def fold[T](dependencies: Set[ReSource[S]], init: T)
             (expr: StaticTicket[S] => (() => T) => T)
             (implicit ticket: CreationTicket[S])
  : Signal[T] = {
    val res = ticket.create(
      dependencies,
      Initializer.InitializedSignal[Pulse[T]](Pulse.tryCatch(Pulse.Value(init))),
      inite = false) {
      state => new SignalImpl[T](state, (st, v) => expr(st)(v), ticket.rename, None)
    }
    rescalaAPI.Signals.wrapWithSignalAPI(res)
  }

  /** Folds when any one of a list of events occurs, if multiple events occur, every fold is executed in order. */
  @cutOutOfUserComputation
  final def foldAll[A](init: A)
                      (accthingy: (=> A) => Seq[FoldMatch[A]])
                      (implicit ticket: CreationTicket[S])
  : Signal[A] = {
    var acc          = () => init
    val ops          = accthingy(acc())
    val staticInputs = ops.collect {
      case StaticFoldMatch(ev, _)        => ev
      case StaticFoldMatchDynamic(ev, _) => ev
    }.toSet

    def operator(dt: DynamicTicket[S], oldValue: () => A): A = {
      acc = oldValue

      def applyToAcc[T](f: T => A, value: Option[T]): Unit = {
        value.foreach { v =>
          val res = f(v)
          acc = () => res
        }
      }

      ops.foreach {
        case StaticFoldMatch(ev, f)        =>
          applyToAcc(f, dt.dependStatic(ev))
        case StaticFoldMatchDynamic(ev, f) =>
          applyToAcc(f(dt), dt.dependStatic(ev))
        case DynamicFoldMatch(evs, f)      =>
          evs().map(dt.depend).foreach {applyToAcc(f, _)}
      }
      acc()
    }

    val res = ticket.create(
      staticInputs.toSet[ReSource[S]],
      Initializer.InitializedSignal[Pulse[A]](Pulse.tryCatch(Pulse.Value(init))),
      inite = true) {
      state => new SignalImpl[A](state, operator, ticket.rename, Some(staticInputs.toSet[ReSource[S]]))
    }
    rescalaAPI.Signals.wrapWithSignalAPI(res)

  }

  val Match = Seq

  sealed trait FoldMatch[+A]
  case class StaticFoldMatch[T, +A](event: Event[T, S], f: T => A) extends FoldMatch[A]
  case class StaticFoldMatchDynamic[T, +A](event: Event[T, S], f: DynamicTicket[S] => T => A) extends FoldMatch[A]
  case class DynamicFoldMatch[T, +A](event: () => Seq[Event[T, S]], f: T => A) extends FoldMatch[A]

  class EOps[T](e: Event[T, S]) {
    /** Constructs a pair similar to ->, however this one is compatible with type inference for [[fold]] */
    final def >>[A](fun: T => A): FoldMatch[A] = StaticFoldMatch(e, fun)
    final def >>>[A](fun: DynamicTicket[S] => T => A): FoldMatch[A] = StaticFoldMatchDynamic(e, fun)
  }
  class ESeqOps[T](e: => Seq[Event[T, S]]) {
    /** Constructs a pair similar to ->, however this one is compatible with type inference for [[fold]] */
    final def >>[A](fun: T => A): FoldMatch[A] = DynamicFoldMatch(() => e, fun)
  }

  case class CBResult[T, R](event: Event[T, S], value: R)
  final class FromCallbackT[T] private[Events](val dummy: Boolean = true) {
    def apply[R](body: (T => Unit) => R)
                (implicit ct: CreationTicket[S], s: Scheduler[S]): CBResult[T, R] = {
      val evt = rescalaAPI.Evt[T]()(ct)
      val res = body(evt.fire)
      CBResult(evt, res)
    }
  }

  def fromCallback[T]: FromCallbackT[T] = new FromCallbackT[T]()
}
