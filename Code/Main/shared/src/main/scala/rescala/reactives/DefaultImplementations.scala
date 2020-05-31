package rescala.reactives

import rescala.core.Pulse.NoChange
import rescala.core.{Base, Derived, DisconnectableImpl, Pulse, REName, ReevTicket, Result, Struct}
import rescala.interface.RescalaInterface
import rescala.reactives.Events.Estate
import rescala.reactives.Signals.{Diff, Sstate}
import rescala.reactives

trait DefaultImplementations[S <: Struct] {

  val rescalaAPI: RescalaInterface[S]

  import rescalaAPI._

  class SignalImpl[T](initial: Sstate[T, S],
                      expr: (DynamicTicket, () => T) => T,
                      name: REName,
                      staticDeps: Option[Set[ReSource]],
                      override val rescalaAPI: RescalaInterface[S])
    extends Base[Pulse[T], S](initial, name) with Derived[S] with Signal[T] with DisconnectableImpl[S] {

    private def computeNewValue(rein: ReevTicket[Pulse[T], S], newValue: () => T): ReevTicket[Pulse[T], S] = {
      val newPulse = Pulse.tryCatch(Pulse.diffPulse(newValue(), rein.before))
      if (newPulse.isChange) rein.withValue(newPulse) else rein
    }

    override protected[rescala] def reevaluate(rein: ReIn): Rout = guardReevaluate(rein) {
      val rein2 = staticDeps.fold(rein.trackStatic())(rein.trackDependencies)
      computeNewValue(rein2, () => expr(rein2, () => rein2.before.get))
    }
  }


  class ChangeEventImpl[T](_bud: S#State[(Pulse[T], Pulse[Diff[T]]), S],
                           signal: reactives.Signal[T, S],
                           name: REName,
                           override val rescalaAPI: RescalaInterface[S])
    extends Base[(Pulse[T], Pulse[Diff[T]]), S](_bud, name) with Derived[S] with Event[Diff[T]] with DisconnectableImpl[S] {

    override type Value = (Pulse[T], Pulse[Diff[T]])


    override def internalAccess(v: (Pulse[T], Pulse[Diff[T]])): Pulse[Diff[T]] = v._2
    override def interpret(v: Value): Option[Diff[T]] = v._2.toOption

    override protected[rescala] def reevaluate(rein: ReIn): Rout = guardReevaluate(rein) {
      val to  : Pulse[T] = rein.collectStatic(signal)
      val from: Pulse[T] = rein.before._1
      if (to == Pulse.empty) return rein // ignore empty propagations
      if (from != Pulse.NoChange) rein.withValue((to, Pulse.Value(Diff(from, to))))
      else rein.withValue((to, Pulse.NoChange)).withPropagate(false)
    }
  }

  class EventImpl[T](_bud: Estate[S, T],
                     expr: DynamicTicket => Pulse[T],
                     name: REName,
                     /** If this is None, the event is static. Else, it is dynamic with the set of static dependencies */
                     isDynamicWithStaticDeps: Option[Set[ReSource]],
                     override val rescalaAPI: RescalaInterface[S])
    extends Base[Pulse[T], S](_bud, name) with Derived[S] with Event[T] with DisconnectableImpl[S] {

    override def internalAccess(v: Pulse[T]): Pulse[T] = v
    override protected[rescala] def reevaluate(rein: ReIn): Rout = guardReevaluate(rein) {
      val rein2 = isDynamicWithStaticDeps.fold(rein.trackStatic())(rein.trackDependencies)
      val value = Pulse.tryCatch(expr(rein2), onEmpty = NoChange)
      if (value.isChange) rein2.withValue(value)
      else rein2
    }
  }
}
