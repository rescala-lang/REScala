package rescala.reactives

import rescala.core.Pulse.NoChange
import rescala.core._
import rescala.macros.cutOutOfUserComputation
import rescala.reactives.Events.Estate
import rescala.reactives.Signals.Diff

/** Functions to construct events, you probably want to use the operators on [[Event]] for a nicer API. */
object Events {
  type Estate[S <: Struct, T] = S#State[Pulse[T], S]


  /** the basic method to create static events */
  @cutOutOfUserComputation
  def staticNamed[T, S <: Struct](name: String,
                                  dependencies: ReSource[S]*)
                                 (calculate: StaticTicket[S] => Pulse[T])
                                 (implicit ticket: CreationTicket[S]): Event[T, S] = {
    ticket.create[Pulse[T], StaticEvent[T, S]](dependencies.toSet, Initializer.Event, inite = false) {
      state => new StaticEvent[T, S](state, calculate, name) with DisconnectableImpl[S]
    }
  }

  /** Creates static events */
  @cutOutOfUserComputation
  def static[T, S <: Struct](dependencies: ReSource[S]*)
                            (calculate: StaticTicket[S] => Option[T])
                            (implicit ticket: CreationTicket[S]): Event[T, S] =
    staticNamed(ticket.rename.str, dependencies: _*)(st => Pulse.fromOption(calculate(st)))

  /** Creates dynamic events */
  @cutOutOfUserComputation
  def dynamic[T, S <: Struct](dependencies: ReSource[S]*)
                             (expr: DynamicTicket[S] => Option[T])
                             (implicit ticket: CreationTicket[S]): Event[T, S] = {
    val staticDeps = dependencies.toSet
    ticket.create[Pulse[T], DynamicEvent[T, S]](staticDeps, Initializer.Event, inite = true) { state =>
      new DynamicEvent[T, S](state, expr.andThen(Pulse.fromOption), ticket.rename, staticDeps)
          with DisconnectableImpl[S]
    }
  }

  object MapFuncImpl { def apply[T1, A](value: Option[T1], mapper: T1 => A): Option[A] =
    value.map(mapper)}
  object FilterFuncImpl { def apply[T1, A](value: Option[T1], filter: T1 => Boolean): Option[T1] =
    value.filter(filter)}
  object CollectFuncImpl { def apply[T1, A](value: Option[T1], filter: PartialFunction[T1, A]): Option[A] =
    value.collect(filter)
  }
  object FoldFuncImpl{ def apply[T1, A](state: () => A, value: Option[T1], step: (A, T1) => A): A =
    value match {
      case None => state()
      case Some(v) => step(state(), v)
    }
  }

  /** Creates change events */
  @cutOutOfUserComputation
  def change[T, S <: Struct](signal: Signal[T, S])(implicit ticket: CreationTicket[S]): Event[Diff[T], S]
  = ticket.transaction { initTurn =>
    val internal = initTurn.create[(Pulse[T], Pulse[Diff[T]]), ChangeEvent[T, S]](
      Set[ReSource[S]](signal), Initializer.Change, inite = true, ticket) { state =>
      new ChangeEvent[T, S](state, signal, ticket.rename) with DisconnectableImpl[S]
    }
    Events.static(internal)(st => st.dependStatic(internal))(initTurn)
  }

  @cutOutOfUserComputation
  def foldOne[A, T, S <: Struct](dependency: Event[A, S], init: T)(expr: (T, A) => T)(implicit ticket: CreationTicket[S]): Signal[T, S] = {
    fold(Set[ReSource[S]](dependency), init){st => acc =>
      val a: A = dependency.internalAccess(st.collectStatic(dependency)).get
      expr(acc(), a)}
  }

  /** Folds events with a given operation to create a Signal.
    *
    * @see [[rescala.reactives.Event.fold]]*/
  @cutOutOfUserComputation
  def fold[T, S <: Struct](dependencies: Set[ReSource[S]], init: T)
                                          (expr: StaticTicket[S] => (() => T) => T)
                                          (implicit ticket: CreationTicket[S])
  : Signal[T, S] = {
    ticket.create(
      dependencies,
      Initializer.InitializedSignal[Pulse[T]](Pulse.tryCatch(Pulse.Value(init))),
      inite = false) {
      state => new StaticSignal[T, S](state, (st, v) => expr(st)(v), ticket.rename)
    }
  }

  /** Folds when any one of a list of events occurs, if multiple events occur, every fold is executed in order. */
  @cutOutOfUserComputation
  final def foldAll[A, S <: Struct](init: A)
                                                   (accthingy: (=> A) => Seq[FoldMatch[A, S]])
                                                   (implicit ticket: CreationTicket[S]): Signal[A, S] = {
    var acc = () => init
    val ops = accthingy(acc())
    val staticInputs = ops.collect {
      case StaticFoldMatch(ev, _) => ev
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
        case StaticFoldMatch(ev, f)   =>
          applyToAcc(f, dt.dependStatic(ev))
        case StaticFoldMatchDynamic(ev, f)   =>
          applyToAcc(f(dt), dt.dependStatic(ev))
        case DynamicFoldMatch(evs, f) =>
          evs().map(dt.depend).foreach {applyToAcc(f, _)}
      }
      acc()
    }

    ticket.create(
      staticInputs.toSet[ReSource[S]],
      Initializer.InitializedSignal[Pulse[A]](Pulse.tryCatch(Pulse.Value(init))),
      inite = true) {
      state => new DynamicSignal[A, S](state, operator, ticket.rename, staticInputs.toSet[ReSource[S]])
    }
  }

  val Match = Seq

  sealed trait FoldMatch[+A, S <: Struct]
  case class StaticFoldMatch[T, +A, S <: Struct](event: Event[T, S], f: T => A) extends FoldMatch[A, S]
  case class StaticFoldMatchDynamic[T, +A, S <: Struct](event: Event[T, S], f: DynamicTicket[S] => T => A) extends FoldMatch[A, S]
  case class DynamicFoldMatch[T, +A, S <: Struct](event: () => Seq[Event[T, S]], f: T => A) extends FoldMatch[A, S]

  class EOps[T, S <: Struct](e: Event[T, S]) {
    /** Constructs a pair similar to ->, however this one is compatible with type inference for [[fold]] */
    final def >>[A](fun: T => A): FoldMatch[A, S] = StaticFoldMatch(e, fun)
    final def >>>[A](fun: DynamicTicket[S] => T => A): FoldMatch[A, S] = StaticFoldMatchDynamic(e, fun)
  }
  class ESeqOps[T, S <: Struct](e: => Seq[Event[T, S]]) {
    /** Constructs a pair similar to ->, however this one is compatible with type inference for [[fold]] */
    final def >>[A](fun: T => A): FoldMatch[A, S] = DynamicFoldMatch(e _, fun)
  }

  def noteFromPulse[N, S <: Struct](t: ReevTicket[Pulse[N], S], value: Pulse[N]): Result[Pulse[N], S] = {
    if (value.isChange) t.withValue(value)
    else t
  }


  case class CBResult[S <: Struct, T, R](event: Event[T, S], value: R)
  final class FromCallbackT[T] private[Events](val dummy: Boolean = true) extends AnyVal {
    def apply[S <: Struct, R](body: (T => Unit) => R)
                             (implicit ct: CreationTicket[S], s: Scheduler[S]): CBResult[S, T, R] = {
      val evt = Evt[T, S]
      val res = body(evt.fire)
      CBResult(evt, res)
    }
  }

  def fromCallback[T]: FromCallbackT[T] = new FromCallbackT[T]()
}


private abstract class StaticEvent[T, S <: Struct](_bud: Estate[S, T],
                                                   expr: StaticTicket[S] => Pulse[T],
                                                   name: REName)
  extends Base[Pulse[T], S](_bud, name) with Derived[S] with Event[T, S] {
  override def internalAccess(v: Pulse[T]): Pulse[T] = v
  override protected[rescala] def reevaluate(rein: ReIn): Rout =
    Events.noteFromPulse[T, S](rein, Pulse.tryCatch(expr(rein), onEmpty = NoChange))
}


private abstract class ChangeEvent[T, S <: Struct](_bud: S#State[(Pulse[T], Pulse[Diff[T]]), S],
                                                   signal: Signal[T, S],
                                                   name: REName)
  extends Base[(Pulse[T], Pulse[Diff[T]]), S](_bud, name) with Derived[S] with Event[Diff[T], S] {

  override type Value = (Pulse[T], Pulse[Diff[T]])


  override def internalAccess(v: (Pulse[T], Pulse[Diff[T]])): Pulse[Diff[T]] =  v._2
  override def interpret(v: Value): Option[Diff[T]] = v._2.toOption

  override protected[rescala] def reevaluate(rein: ReIn): Rout = {
    val to: Pulse[T] = rein.collectStatic(signal)
    val from: Pulse[T] = rein.before._1
    if (to == Pulse.empty) return rein // ignore empty propagations
    if (from != Pulse.NoChange) rein.withValue((to, Pulse.Value(Diff(from, to))))
    else rein.withValue((to, Pulse.NoChange)).withPropagate(false)
  }
}

private abstract class DynamicEvent[T, S <: Struct](_bud: Estate[S, T],
                                                    expr: DynamicTicket[S] => Pulse[T],
                                                    name: REName,
                                                    staticDeps: Set[ReSource[S]])
  extends Base[Pulse[T], S](_bud, name) with Derived[S] with Event[T, S] {

  override def internalAccess(v: Pulse[T]): Pulse[T] = v
  override protected[rescala] def reevaluate(rein: ReIn): Rout = {
    rein.trackDependencies(staticDeps)
    Events.noteFromPulse[T, S](rein, Pulse.tryCatch(expr(rein), onEmpty = NoChange))
  }
}
