package reactives.structure

import reactives.SelectedScheduler.State
import reactives.core.*

class ChangeEventImpl[T](
    initial: State[(Pulse[T], Pulse[Diff[T]])],
    signal: ReSource.of[State] { type Value <: Pulse[T] },
    name: ReInfo
) extends Base[(Pulse[T], Pulse[Diff[T]])](initial, name)
    with Derived
    with DisconnectableImpl {

  override type Value = (Pulse[T], Pulse[Diff[T]])

  override protected[reactives] def commit(base: (Pulse[T], Pulse[Diff[T]])): (Pulse[T], Pulse[Diff[T]]) =
    base.copy(_2 = Pulse.NoChange)

  def internalAccess(v: (Pulse[T], Pulse[Diff[T]])): Pulse[Diff[T]] = v._2

  override protected[reactives] def guardedReevaluate(rein: ReIn): Rout = {
    val to: Pulse[T]   = rein.collectStatic(signal)
    val from: Pulse[T] = rein.before._1
    if Pulse.empty.unapply(to).isDefined then rein // ignore empty propagations
    else if from != Pulse.NoChange then rein.withValue((to, Pulse.Value(Diff(from, to))))
    else rein.withValue((to, Pulse.NoChange)).withPropagate(false)
  }
}
