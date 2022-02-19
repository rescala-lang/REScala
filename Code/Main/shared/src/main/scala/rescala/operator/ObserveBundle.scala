package rescala.operator

import rescala.core.Core

trait ObserveBundle extends Core {

  /** Generic interface for observers that represent a function registered to trigger for every reevaluation of a reactive value.
    * Currently this interface is only used to allow a removal of registered observer functions.
    */
  trait Observe {
    def remove(): Unit
  }

  trait ObserveInteract extends Observation {
    def checkExceptionAndRemoval(): Boolean
  }

  object Observe {
    def strong[T](
        dependency: ReSource,
        fireImmediately: Boolean
    )(fun: dependency.Value => ObserveInteract)(implicit ct: CreationTicket): Observe = {
      ct.create[Pulse[Nothing], Observe with Derived](Set(dependency), Pulse.NoChange, fireImmediately) { state =>
        class Obs extends Base[Pulse[Nothing]](state, ct.rename) with Derived with Observe with DisconnectableImpl {

          override protected[rescala] def commit(base: Obs.this.Value): Obs.this.Value = Pulse.NoChange

          override protected[rescala] def guardedReevaluate(dt: ReIn): Rout = {
            val v  = dt.collectStatic(dependency)
            val oi = fun(v)
            if (oi.checkExceptionAndRemoval()) dt.trackDependencies(Set.empty)
            else dt.withEffect(oi)
          }
          override def remove(): Unit = disconnect()
        }
        new Obs
      }
    }

  }
}
