package rescala.reactives

import rescala.core._

/**
  * Generic interface for observers that represent a function registered to trigger for every reevaluation of a reactive value.
  * Currently this interface is only used to allow a removal of registered observer functions.
  *
  * @tparam S Struct type used for the propagation of the signal
  */
trait Observe[S <: Struct] {
  def remove()(implicit fac: Scheduler[S]): Unit
}

object Observe {

  trait ObserveInteract extends Observation {
    def checkExceptionAndRemoval(): Boolean
  }

  def strong[T, S <: Struct](
      dependency: ReSource[S],
      fireImmediately: Boolean
  )(fun: dependency.Value => ObserveInteract)(implicit ct: CreationTicket[S]): Observe[S] = {
    ct.create[Pulse[Nothing], Observe[S] with Derived[S]](Set(dependency), Pulse.NoChange, fireImmediately) { state =>
      class Obs
          extends Base[Pulse[Nothing], S](state, ct.rename)
          with Derived[S]
          with Observe[S]
          with DisconnectableImpl[S] {

        override protected[rescala] def commit(base: Obs.this.Value): Obs.this.Value = Pulse.NoChange

        override protected[rescala] def reevaluate(dt: ReIn): Rout =
          guardReevaluate(dt) {
            val v  = dt.collectStatic(dependency)
            val oi = fun(v)
            if (oi.checkExceptionAndRemoval()) dt.trackDependencies(Set.empty)
            else dt.withEffect(oi)
          }
        override def remove()(implicit fac: Scheduler[S]): Unit = disconnect()
      }
      new Obs
    }
  }

}
