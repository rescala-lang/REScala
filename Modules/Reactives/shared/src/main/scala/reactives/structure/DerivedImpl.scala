package reactives.structure

import reactives.core.*
import reactives.operator.Interface
import reactives.operator.Interface.State
import reactives.structure.Pulse.NoChange

abstract class DerivedImpl[T](
    initial: State[Pulse[T]],
    name: ReInfo,
    isDynamicWithStaticDeps: Option[Set[ReSource.of[State]]]
) extends Base[Pulse[T]](initial, name)
    with Derived
    with DisconnectableImpl {

  override type State[V] = Interface.State[V]

  override protected[reactives] def guardedReevaluate(rein: ReIn): Rout = {
    val rein2 = isDynamicWithStaticDeps match {
      case None       => rein.trackStatic()
      case Some(deps) => rein.trackDependencies(deps)
    }
    val newPulse = computePulse(rein2)
    if (newPulse.isChange) rein2.withValue(newPulse) else rein2
  }

  protected def computePulse(rein: ReevTicket[State, Pulse[T]]): Pulse[T]
}

/** @param isDynamicWithStaticDeps None means static dependencies only,
  *                                Some means dynamic with the given static ones for optimization
  */
abstract class SignalImpl[T](
    initial: State[Pulse[T]],
    expr: (DynamicTicket[State], () => T) => T,
    name: ReInfo,
    isDynamicWithStaticDeps: Option[Set[ReSource.of[State]]]
) extends DerivedImpl[T](initial, name, isDynamicWithStaticDeps) {

  protected def computePulse(rein: ReevTicket[State, Pulse[T]]): Pulse[T] = {
    Pulse.tryCatch(Pulse.diffPulse(expr(rein, () => rein.before.get), rein.before))
  }
}

/** @param isDynamicWithStaticDeps If this is None, the event is static. Else, it is dynamic with the set of static dependencies */
class EventImpl[T](
    initial: State[Pulse[T]],
    expr: DynamicTicket[State] => Pulse[T],
    name: ReInfo,
    isDynamicWithStaticDeps: Option[Set[ReSource.of[State]]]
) extends DerivedImpl[T](initial, name, isDynamicWithStaticDeps) {

  override protected[reactives] def commit(base: Pulse[T]): Pulse[T] = Pulse.NoChange

  /** This implements an abstract method on [[reactives.operator.Event]] */
  def internalAccess(v: Pulse[T]): Pulse[T] = v

  override protected def computePulse(rein: ReevTicket[State, Pulse[T]]): Pulse[T] =
    Pulse.tryCatch(expr(rein), onEmpty = _ => NoChange)
}
