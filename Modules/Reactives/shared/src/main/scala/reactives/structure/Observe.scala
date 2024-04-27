package reactives.structure

import reactives.core.*
import reactives.operator.Interface.State

/** observers are normal reactives that are configured by a function that converts the value of the input into an [[ObserveInteract]] */
object Observe {

  trait ObserveInteract extends Observation {
    // if true, the observer will remove all of its inputs, which allows eventual collection
    def checkExceptionAndRemoval(): Boolean
  }

  def strong[T](
      dependency: ReSource.of[State],
      fireImmediately: Boolean
  )(fun: dependency.Value => ObserveInteract)(implicit ct: CreationTicket[dependency.State]): Disconnectable = {
    ct.scope.create[Pulse[Nothing], Disconnectable & Derived.of[State]](
      Set(dependency),
      Pulse.NoChange,
      fireImmediately
    ) {
      state =>
        class Obs extends Base[Pulse[Nothing]](state, ct.info) with Derived
            with DisconnectableImpl {

          override protected[reactives] def commit(base: Obs.this.Value): Obs.this.Value = Pulse.NoChange

          override protected[reactives] def guardedReevaluate(dt: ReIn): Rout = {
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
