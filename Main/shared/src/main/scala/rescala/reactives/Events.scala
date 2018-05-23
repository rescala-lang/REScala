package rescala.reactives

import rescala.core.Pulse.NoChange
import rescala.core._
import rescala.macros.cutOutOfUserComputation
import rescala.reactives.Events.Estate
import rescala.reactives.Signals.Diff

import scala.language.existentials

/** Functions to construct events, you probably want to use the operators on [[Event]] for a nicer API. */
object Events {
  type Estate[S <: Struct, T] = S#State[Pulse[T], S]


  /** the basic method to create static events */
  @cutOutOfUserComputation
  def staticNamed[T, S <: Struct](name: String,
                                  dependencies: ReSource[S]*)
                                 (calculate: StaticTicket[S] => Pulse[T])
                                 (implicit ticket: CreationTicket[S]): Event[T, S] = ticket { initTurn =>
    initTurn.create[Pulse[T], StaticEvent[T, S]](dependencies.toSet, Initializer.Event, inite = false) {
      state => new StaticEvent[T, S](state, calculate, name) with DisconnectableImpl[S]
    }
  }

  /** Creates static events */
  @cutOutOfUserComputation
  def static[T, S <: Struct](dependencies: ReSource[S]*)
                            (calculate: StaticTicket[S] => Option[T])
                            (implicit ticket: CreationTicket[S]): Event[T, S] =
    staticNamed(ticket.rename.name, dependencies: _*)(st => Pulse.fromOption(calculate(st)))

  /** Creates dynamic events */
  @cutOutOfUserComputation
  def dynamic[T, S <: Struct](dependencies: ReSource[S]*)(expr: DynamicTicket[S] => Option[T])(implicit ticket: CreationTicket[S]): Event[T, S] = {
    ticket { initialTurn =>
      val staticDeps = dependencies.toSet
      initialTurn.create[Pulse[T], DynamicEvent[T, S]](staticDeps, Initializer.Event, inite = true) {
        state => new DynamicEvent[T, S](state, expr.andThen(Pulse.fromOption), ticket.rename, staticDeps) with DisconnectableImpl[S]
      }
    }
  }

  /** Creates change events */
  @cutOutOfUserComputation
  def change[T, S <: Struct](signal: Signal[T, S])(implicit ticket: CreationTicket[S]): Event[Diff[T], S] = ticket { initTurn =>
    val internal = initTurn.create[(Pulse[T], Pulse[Diff[T]]), ChangeEvent[T, S]](
      Set[ReSource[S]](signal), Initializer.Change, inite = true) { state =>
      new ChangeEvent[T, S](state, signal, ticket.rename) with DisconnectableImpl[S]
    }
    Events.static(internal)(st => st.dependStatic(internal))(initTurn)
  }

  @cutOutOfUserComputation
  def foldOne[A, T: ReSerializable, S <: Struct](dependency: Event[A, S], init: T)(expr: (T, A) => T)(implicit ticket: CreationTicket[S]): Signal[T, S] = {
    fold(Set[ReSource[S]](dependency), init){(st, acc) =>
      val a: A = dependency.internalAccess(st.collectStatic(dependency)).get
      expr(acc(), a)}
  }

  /** Folds events with a given operation to create a Signal.
    *
    * @see [[Event.fold]]*/
  @cutOutOfUserComputation
  def fold[T: ReSerializable, S <: Struct](dependencies: Set[ReSource[S]], init: T)(expr: (StaticTicket[S], () => T) => T)(implicit ticket: CreationTicket[S]): Signal[T, S] = {
    ticket { initialTurn =>
      initialTurn.create[Pulse[T], StaticSignal[T, S]](dependencies,
        Initializer.InitializedSignal[Pulse[T]](Pulse.tryCatch(Pulse.Value(init))), inite = false) {
        state => new StaticSignal[T, S](state, expr, ticket.rename) with DisconnectableImpl[S]
      }
    }
  }

  /** Folds when any one of a list of events occurs, if multiple events occur, every fold is executed in order. */
  @cutOutOfUserComputation
  final def foldAll[A: ReSerializable, S <: Struct](init: A)
                                                   (accthingy: (=> A) => Seq[(Event[T, S], T => A) forSome {type T}])
                                                   (implicit ticket: CreationTicket[S]): Signal[A, S] = {
    ticket { initialTurn =>
      var acc = () => init
      val ops = accthingy(acc())
      val dependencies = ops.map(_._1)
      fold[A, S](dependencies.toSet[ReSource[S]], init) { (st, currentValue) =>
        acc = currentValue
        for ((ev, f) <- ops) {
          val value = st.dependStatic(ev)
          value.foreach { v =>
            val res = f(v)
            acc = () => res
          }
        }
        acc()
      }(implicitly[ReSerializable[A]], CreationTicket.fromCreation(initialTurn)(ticket.rename))
    }
  }

  final def Match[S <: Struct, A](ops: (Event[T, S], T => A) forSome {type T}*): Seq[((Event[T, S], T => A)) forSome {type T}] = ops

  class EOps[T, S <: Struct](val e: Event[T, S]) {
    /** Constructs a pair similar to ->, however this one is compatible with type inference for [[fold]] */
    final def >>[A](fun: T => A): (Event[T, S], T => A) = (e, fun)
  }

  def noteFromPulse[N, S <: Struct](t: ReevTicket[Pulse[N], S], value: Pulse[N]): Result[Pulse[N], S] = {
    if (value.isChange) t.withValue(value)
    else t
  }
}


private abstract class StaticEvent[T, S <: Struct](_bud: Estate[S, T], expr: StaticTicket[S] => Pulse[T], name: REName)
  extends Base[Pulse[T], S](_bud, name) with Event[T, S] {
  override def internalAccess(v: Pulse[T]): Pulse[T] = v
  override protected[rescala] def reevaluate(rein: ReIn): Rout =
    Events.noteFromPulse[T, S](rein, Pulse.tryCatch(expr(rein), onEmpty = NoChange))
}


private abstract class ChangeEvent[T, S <: Struct](_bud: S#State[(Pulse[T], Pulse[Diff[T]]), S], signal: Signal[T, S], name: REName)
  extends Base[(Pulse[T], Pulse[Diff[T]]), S](_bud, name) with Event[Diff[T], S] {

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

private abstract class DynamicEvent[T, S <: Struct](_bud: Estate[S, T], expr: DynamicTicket[S] => Pulse[T], name: REName, staticDeps: Set[ReSource[S]])
  extends Base[Pulse[T], S](_bud, name) with Event[T, S] {

  override def internalAccess(v: Pulse[T]): Pulse[T] = v
  override protected[rescala] def reevaluate(rein: ReIn): Rout = {
    rein.trackDependencies(staticDeps)
    Events.noteFromPulse[T, S](rein, Pulse.tryCatch(expr(rein), onEmpty = NoChange))
  }
}
