package rescala.structure

import rescala.core.*

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
