package rescala.structure

import rescala.core.{DynamicTicket, ReInfo, ReSource, ReevTicket}
import rescala.structure.Pulse.NoChange

/** @param isDynamicWithStaticDeps If this is None, the event is static. Else, it is dynamic with the set of static dependencies */
class EventImpl[State[_], T](
    initial: State[Pulse[T]],
    expr: DynamicTicket[State] => Pulse[T],
    name: ReInfo,
    isDynamicWithStaticDeps: Option[Set[ReSource.of[State]]]
) extends DerivedImpl[State, T](initial, name, isDynamicWithStaticDeps) {

  override protected[rescala] def commit(base: Pulse[T]): Pulse[T] = Pulse.NoChange

  def internalAccess(v: Pulse[T]): Pulse[T] = v

  override protected[this] def computePulse(rein: ReevTicket[State, Pulse[T]]): Pulse[T] =
    Pulse.tryCatch(expr(rein), onEmpty = NoChange)
}
