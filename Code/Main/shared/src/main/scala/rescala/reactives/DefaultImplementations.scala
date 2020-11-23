package rescala.reactives

import rescala.core.Pulse.NoChange
import rescala.core._
import rescala.interface.RescalaInterface
import rescala.reactives
import rescala.reactives.Events.Estate
import rescala.reactives.Signals.{SignalResource, Sstate}

trait DefaultImplementations[S <: Struct] {
  class SignalImpl[T](
      initial: Sstate[T, S],
      expr: (DynamicTicket[S], () => T) => T,
      name: ReName,
      isDynamicWithStaticDeps: Option[Set[ReSource[S]]]
  ) extends DerivedImpl[T](initial, name, isDynamicWithStaticDeps)
      with SignalResource[T, S] {

    protected[this] def computePulse(rein: ReevTicket[Pulse[T], S]): Pulse[T] = {
      Pulse.tryCatch(Pulse.diffPulse(expr(rein, () => rein.before.get), rein.before))
    }
  }

  abstract class DerivedImpl[T](initial: Sstate[T, S], name: ReName, isDynamicWithStaticDeps: Option[Set[ReSource[S]]])
      extends Base[Pulse[T], S](initial, name)
      with Derived[S]
      with DisconnectableImpl[S] {

    override protected[rescala] def reevaluate(rein: ReIn): Rout =
      guardReevaluate(rein) {
        val rein2    = isDynamicWithStaticDeps.fold(rein.trackStatic())(rein.trackDependencies)
        val newPulse = computePulse(rein2)
        if (newPulse.isChange) rein2.withValue(newPulse) else rein2
      }
    protected[this] def computePulse(rein: ReevTicket[Pulse[T], S]): Pulse[T]
  }

  class EventImpl[T](
      initial: Estate[S, T],
      expr: DynamicTicket[S] => Pulse[T],
      name: ReName,
      /** If this is None, the event is static. Else, it is dynamic with the set of static dependencies */
      isDynamicWithStaticDeps: Option[Set[ReSource[S]]],
      override val rescalaAPI: RescalaInterface[S]
  ) extends DerivedImpl[T](initial, name, isDynamicWithStaticDeps)
      with Event[T, S] {

    override protected[rescala] def commit(base: Pulse[T]): Pulse[T] = Pulse.NoChange
    override def internalAccess(v: Pulse[T]): Pulse[T]               = v
    override protected[this] def computePulse(rein: ReevTicket[Pulse[T], S]): Pulse[T] =
      Pulse.tryCatch(expr(rein), onEmpty = NoChange)
  }

  class ChangeEventImpl[T](
      _bud: S#State[(Pulse[T], Pulse[Diff[T]]), S],
      signal: reactives.Signal[T, S],
      name: ReName,
      override val rescalaAPI: RescalaInterface[S]
  ) extends Base[(Pulse[T], Pulse[Diff[T]]), S](_bud, name)
      with Derived[S]
      with Event[Diff[T], S]
      with DisconnectableImpl[S] {

    override type Value = (Pulse[T], Pulse[Diff[T]])

    override protected[rescala] def commit(base: (Pulse[T], Pulse[Diff[T]])): (Pulse[T], Pulse[Diff[T]]) =
      base.copy(_2 = Pulse.NoChange)
    override def internalAccess(v: (Pulse[T], Pulse[Diff[T]])): Pulse[Diff[T]] = v._2
    override def interpret(v: Value): Option[Diff[T]]                          = v._2.toOption

    override protected[rescala] def reevaluate(rein: ReIn): Rout =
      guardReevaluate(rein) {
        val to: Pulse[T]   = rein.collectStatic(signal.resource)
        val from: Pulse[T] = rein.before._1
        if (to == Pulse.empty) rein // ignore empty propagations
        else if (from != Pulse.NoChange) rein.withValue((to, Pulse.Value(Diff(from, to))))
        else rein.withValue((to, Pulse.NoChange)).withPropagate(false)
      }
  }

}
