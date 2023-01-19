package rescala.operator

import rescala.core._
import rescala.operator.Pulse.NoChange

trait DefaultImplementations {
  self: Operators =>

  /** @param isDynamicWithStaticDeps None means static dependencies only,
    *                                Some means dynamic with the given static ones for optimization
    */
  class SignalImpl[T](
      initial: State[Pulse[T]],
      expr: (DynamicTicket, () => T) => T,
      name: ReName,
      isDynamicWithStaticDeps: Option[Set[ReSource.of[State]]]
  ) extends DerivedImpl[T](initial, name, isDynamicWithStaticDeps) with Signal[T] {

    protected[this] def computePulse(rein: ReevTicket[State, Pulse[T]]): Pulse[T] = {
      Pulse.tryCatch(Pulse.diffPulse(expr(rein, () => rein.before.get), rein.before))
    }
  }

  abstract class DerivedImpl[T](initial: State[Pulse[T]], name: ReName, isDynamicWithStaticDeps: Option[Set[ReSource.of[State]]])
      extends Base[State, Pulse[T]](initial, name)
      with Derived
      with DisconnectableImpl {

    override type State[V] = self.State[V]

    override protected[rescala] def guardedReevaluate(rein: ReIn): Rout = {
      val rein2    =  isDynamicWithStaticDeps match {
        case None => rein.trackStatic()
        case Some(deps) => rein.trackDependencies(deps)
      }
      val newPulse = computePulse(rein2)
      if (newPulse.isChange) rein2.withValue(newPulse) else rein2
    }
    protected[this] def computePulse(rein: ReevTicket[State, Pulse[T]]): Pulse[T]
  }

  /** @param isDynamicWithStaticDeps If this is None, the event is static. Else, it is dynamic with the set of static dependencies */
  class EventImpl[T](
      initial: State[Pulse[T]],
      expr: self.DynamicTicket => Pulse[T],
      name: ReName,
      isDynamicWithStaticDeps: Option[Set[ReSource.of[State]]]
  ) extends DerivedImpl[T](initial, name, isDynamicWithStaticDeps)
      with Event[T] {

    override protected[rescala] def commit(base: Pulse[T]): Pulse[T] = Pulse.NoChange
    override def internalAccess(v: Pulse[T]): Pulse[T]               = v
    override protected[this] def computePulse(rein: ReevTicket[State, Pulse[T]]): Pulse[T] =
      Pulse.tryCatch(expr(rein), onEmpty = NoChange)
  }

  class ChangeEventImpl[T](
      _bud: State[(Pulse[T], Pulse[Diff[T]])],
      signal: Signal[T],
      name: ReName
  ) extends Base[State, (Pulse[T], Pulse[Diff[T]])](_bud, name)
      with Derived
      with Event[Diff[T]]
      with DisconnectableImpl {

    override type Value = (Pulse[T], Pulse[Diff[T]])

    override protected[rescala] def commit(base: (Pulse[T], Pulse[Diff[T]])): (Pulse[T], Pulse[Diff[T]]) =
      base.copy(_2 = Pulse.NoChange)
    override def internalAccess(v: (Pulse[T], Pulse[Diff[T]])): Pulse[Diff[T]] = v._2
    override def read(v: Value): Option[Diff[T]]                               = v._2.toOption

    override protected[rescala] def guardedReevaluate(rein: ReIn): Rout = {
      val to: Pulse[T]   = rein.collectStatic(signal)
      val from: Pulse[T] = rein.before._1
      if (to == Pulse.empty) rein // ignore empty propagations
      else if (from != Pulse.NoChange) rein.withValue((to, Pulse.Value(Diff(from, to))))
      else rein.withValue((to, Pulse.NoChange)).withPropagate(false)
    }
  }

}
