package rescala.structures

import rescala.core.*
import rescala.structures.Pulse.NoChange

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






