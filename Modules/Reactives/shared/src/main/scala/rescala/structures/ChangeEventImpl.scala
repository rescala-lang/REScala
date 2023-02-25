package rescala.structures

import rescala.core.*

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
