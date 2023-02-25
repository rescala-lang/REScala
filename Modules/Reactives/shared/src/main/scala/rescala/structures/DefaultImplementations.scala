package rescala.structures

import rescala.core.*
import Pulse.NoChange

/** @param isDynamicWithStaticDeps None means static dependencies only,
  *                                Some means dynamic with the given static ones for optimization
  */
abstract class SignalImpl[S[_], T](
    initial: S[Pulse[T]],
    expr: (DynamicTicket[S], () => T) => T,
    name: ReInfo,
    isDynamicWithStaticDeps: Option[Set[ReSource.of[S]]]
) extends DerivedImpl[S, T](initial, name, isDynamicWithStaticDeps) {

  protected[this] def computePulse(rein: ReevTicket[S, Pulse[T]]): Pulse[T] = {
    Pulse.tryCatch(Pulse.diffPulse(expr(rein, () => rein.before.get), rein.before))
  }
}

abstract class DerivedImpl[S[_], T](
    initial: S[Pulse[T]],
    name: ReInfo,
    isDynamicWithStaticDeps: Option[Set[ReSource.of[S]]]
) extends Base[S, Pulse[T]](initial, name)
    with Derived
    with DisconnectableImpl {

  override type State[V] = S[V]

  override protected[rescala] def guardedReevaluate(rein: ReIn): Rout = {
    val rein2 = isDynamicWithStaticDeps match {
      case None       => rein.trackStatic()
      case Some(deps) => rein.trackDependencies(deps)
    }
    val newPulse = computePulse(rein2)
    if (newPulse.isChange) rein2.withValue(newPulse) else rein2
  }
  protected[this] def computePulse(rein: ReevTicket[S, Pulse[T]]): Pulse[T]
}

/** @param isDynamicWithStaticDeps If this is None, the event is static. Else, it is dynamic with the set of static dependencies */
class EventImpl[State[_], T](
    initial: State[Pulse[T]],
    expr: DynamicTicket[State] => Pulse[T],
    name: ReInfo,
    isDynamicWithStaticDeps: Option[Set[ReSource.of[State]]]
) extends DerivedImpl[State, T](initial, name, isDynamicWithStaticDeps) {

  override protected[rescala] def commit(base: Pulse[T]): Pulse[T] = Pulse.NoChange
  def internalAccess(v: Pulse[T]): Pulse[T]                        = v
  override protected[this] def computePulse(rein: ReevTicket[State, Pulse[T]]): Pulse[T] =
    Pulse.tryCatch(expr(rein), onEmpty = NoChange)
}

class ChangeEventImpl[S[_], T](
    _bud: S[(Pulse[T], Pulse[Diff[T]])],
    signal: ReSource.of[S] { type Value <: Pulse[T] },
    name: ReInfo
) extends Base[S, (Pulse[T], Pulse[Diff[T]])](_bud, name)
    with Derived
    with DisconnectableImpl {

  override type Value = (Pulse[T], Pulse[Diff[T]])

  override protected[rescala] def commit(base: (Pulse[T], Pulse[Diff[T]])): (Pulse[T], Pulse[Diff[T]]) =
    base.copy(_2 = Pulse.NoChange)
  def internalAccess(v: (Pulse[T], Pulse[Diff[T]])): Pulse[Diff[T]] = v._2

  override protected[rescala] def guardedReevaluate(rein: ReIn): Rout = {
    val to: Pulse[T]   = rein.collectStatic(signal)
    val from: Pulse[T] = rein.before._1
    if (to == Pulse.empty) rein // ignore empty propagations
    else if (from != Pulse.NoChange) rein.withValue((to, Pulse.Value(Diff(from, to))))
    else rein.withValue((to, Pulse.NoChange)).withPropagate(false)
  }
}
