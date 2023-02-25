package rescala.structure

import rescala.core.*

/** observers are normale reactives that are configured by a function that converts the value of the input into an [[ObserveInteract]] */
object Observe {
  def strong[T](
      dependency: ReSource,
      fireImmediately: Boolean
  )(fun: dependency.Value => ObserveInteract)(implicit ct: CreationTicket[dependency.State]): Disconnectable = {
    ct.create[Pulse[Nothing], Disconnectable with Derived.of[dependency.State]](
      Set(dependency),
      Pulse.NoChange,
      fireImmediately
    ) {
      state =>
        class Obs extends Base[dependency.State, Pulse[Nothing]](state, ct.info) with Derived
            with DisconnectableImpl {

          override protected[rescala] def commit(base: Obs.this.Value): Obs.this.Value = Pulse.NoChange

          override protected[rescala] def guardedReevaluate(dt: ReIn): Rout = {
            val v  = dt.collectStatic(dependency)
            val oi = fun(v)
            if (oi.checkExceptionAndRemoval()) dt.trackDependencies(Set.empty)
            else dt.withEffect(oi)
          }
        }
        new Obs
    }
  }

}
